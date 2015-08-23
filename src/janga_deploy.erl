%%
%% Copyright (c) 2013 Ulf Angermann  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

%%% -------------------------------------------------------------------
%%% Author  : Ulf uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------
-module(janga_deploy).

-define(TMPDIR, "/tmp").
-define(ZIPDIR, "zips").
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([deploy/1, undeploy/1, update/1]).
-export([add_path_for_deps/2, download/1, install/2]).

deploy(JApp) ->
	Repo = janga_config:get_env(janga_core, repo_dir),
	Destination = filename:join(filename:absname("japps") , JApp),
	Source = filename:absname(filename:join(filename:absname(Repo), JApp)),
	case file:make_dir(Destination) of
		{error,eexist} -> lager:warning("the japp : ~p already exists.", [JApp]);
		ok -> copy_dir(Source, Destination, JApp, ["messages.config", "service.config"]), 
			  add_path(Destination),
			  Filter = get_deps_config(Destination),
			  add_path_for_deps(Destination, Filter)
	end,
	lager:info("japp : ~p is deployed. Now you can start it.", [JApp]).

undeploy(JApp) ->
	janga_app:stop(JApp),
	JApp_dir = filename:join(filename:absname("japps") , JApp),
	delete_dir(JApp_dir),	
	lager:info("japp : ~p is undeployed.", [JApp]).

update(JApp) ->
	Repo = janga_config:get_env(janga_core, repo_dir),
	Destination = filename:join(filename:absname("japps") , JApp),
	Source = filename:absname(filename:join(filename:absname(Repo), JApp)),
	Configs = get_additional_configs(Destination),
	copy_dir(Source, Destination, JApp, [Configs|["messages.config", "service.config"]]),	
	Filter = get_deps_config(Destination),
	Configs = get_additional_configs(Destination),
	add_path_for_deps(Destination, Filter),
	lager:info("japp : ~p is updated", [JApp]).

download(JApp) ->
	{ok, "200", Header, {file, Name}} = ibrowse:send_req(janga_config:get_env(janga_core, repo_uri) ++ atom_to_list(JApp), [], get, [], [{save_response_to_file, true}]),
	Content_Disposition = proplists:get_value("Content-Disposition", Header),
	FileName = lists:nth(2,string:tokens(lists:nth(2,string:tokens(Content_Disposition, ";")),"=")),
	file:rename(Name, filename:join(["/tmp", FileName])),
	{ok, FileName}.
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
copy_dir(Source, Destination, JApp, Filter) ->
	Files = all_files_from_dir(Source),
	copy_files(Files, Destination, JApp, Filter).

copy_files([], _Destination, _JApp, _Filter) ->
	ok;
copy_files([File|Files], Destination, JApp, Filter) ->
	%%lager:info("file to copy : ~p", [File]),
	case filelib:is_dir(File) of
		true -> create_dir(File, Destination, JApp);
		false -> case is_config_file(filename:join([Destination, extract_rest(File, JApp)]), Filter) of 
					false -> copy_file(File, Destination, JApp, Filter);
					true -> lager:info("we don't overwrite config file: ~p", [File])
				 end
	end,
	copy_files(Files, Destination, JApp, Filter).

create_dir(Dir, Destination, JApp) ->
	%%lager:info("make dir : ~p", [filename:join([Destination, extract_rest(Dir, JApp)])]),
	file:make_dir(filename:join([Destination, extract_rest(Dir, JApp)])).

copy_file(File, Destination, JApp, Filter) ->
	lager:debug("copy_file : ~p", [filename:join([Destination, extract_rest(File, JApp)])]),
	lager:debug("Filter : ~p", [Filter]),
	{ok, _BytesCopied} = file:copy(File, filename:join([Destination, extract_rest(File, JApp)])).



is_config_file(File, Filter) ->
	case lists:member(filename:basename(File) , Filter) of	
		true -> filelib:is_file(File);
		false -> false
	end.

extract_rest(File, JApp) ->
	Start = string:str(File, JApp),
	Length = erlang:length(JApp),
	string:substr(File, Start + Length + 1).

all_files_from_dir(Dir) ->
	filter(filelib:wildcard(Dir ++ "/**/*")).

filter(Files) ->
	[File||File <- Files, is_git_filter(File), is_ds_store_filter(File), is_eunit_filter(File), is_rebar_filter(File)].

is_filter(File, Filter) ->
	case string:str(File, Filter) of
		0 -> true;
		_ -> false
	end.

is_ds_store_filter(File) ->
	is_filter(File, ".DS_Store").

is_git_filter(File) ->
	is_filter(File, ".git").

is_eunit_filter(File) ->
	is_filter(File, ".eunit").

is_rebar_filter(File) ->
	is_filter(File, ".rebar").

add_path(Destination) ->
	true = code:add_pathz(filename:join(Destination, "ebin")). 

add_path_for_deps(Destination, Filter) ->	
	Files = filelib:wildcard(Destination ++ "/deps/*"),	
	[add_path(File)||File <- Files, filelib:is_dir(File), is_to_add(filename:join([Destination,"deps",File]), Filter)].

is_to_add(File, Filter) ->
	case lists:member(File, Filter) of
		true -> true;
		false -> delete_dir(File), false
	end.

get_deps_config(Path_for_japp) ->
	case file:consult(filename:join([Path_for_japp, "deploy.config"])) of
		{ok, Deps} -> create_path_for_deps(Path_for_japp, Deps);
		_ -> []
	end.

get_additional_configs(Path_for_config) ->
	case file:consult(filename:join([Path_for_config, "additional.config"])) of
		{ok, Configs} -> Configs;
		_ -> []
	end.

create_path_for_config(_Path_for_japp, []) ->
	[];
create_path_for_config(Path_for_japp, [Configs]) ->
	[filename:join([Path_for_japp, Config]) || Config <- Configs].

create_path_for_deps(_Path_for_japp, []) ->
	[];
create_path_for_deps(Path_for_japp, Deps) ->
	[filename:join([Path_for_japp, "deps", Dep])||Dep <- lists:flatten(Deps)].

delete_dir(JApp_dir) ->
	Files = all_files_from_dir(JApp_dir), 
	delete_files([JApp_dir|Files], []).
	
delete_files([], Dirs) ->
	delete_dirs(Dirs);
delete_files([File|Files], Dirs) ->
	%%lager:info("file to delete : ~p", [File]),
	N_dirs = case filelib:is_dir(File) of
		true -> [File|Dirs];
		false -> file:delete(File), Dirs 
	end,
	delete_files(Files, N_dirs).

delete_dirs([]) ->
	ok;
delete_dirs([Dir|Dirs]) ->
	file:del_dir(Dir),
	delete_dirs(Dirs). 

install(JApp, ZipFile) ->
	lager:info("starting installing : ~p", [JApp]),
	Repo_path = janga_config:get_env(janga_core, repo_dir),
	ZipFile = backup(atom_to_list(JApp)),
	lager:info("backup : ~p", [ZipFile]),
	delete_dir(filename:join([Repo_path, JApp])),
	extract_zip_to_repo(ZipFile, ?TMPDIR, Repo_path),
	lager:info("finished installing : ~p", [JApp]),
	ok.

extract_zip_to_repo(ZipFile, Tmp_path, Repo_path) ->
	zip:unzip(filename:join([Tmp_path, ZipFile]) , [{cwd, Repo_path}]).

backup(JApp) ->
	Repo_path = janga_config:get_env(janga_core, repo_dir),
	ZipFileName = create_zip_name(JApp, Repo_path),
	ZipFile = create_zip(filename:join([Repo_path, ?ZIPDIR, ZipFileName]), JApp, Repo_path),
	ZipFile.

%%TODO: move it in a central place and use it also from jappsrepo
create_zip(File, Japp, Repo_path) when is_list(File) ->	
	Files = filelib:wildcard(Japp ++ "/**", Repo_path),
	{ok, ZipFile} = zip:zip(File, Files, [{cwd, Repo_path}]),
	ZipFile.

version(Japp, Path) ->
	{ok, [{_A, _B, L}]} = file:consult(filename:join([Path, Japp, "ebin", Japp ++ ".app"])),
	proplists:get_value(vsn, L).

create_zip_name(Japp, Path) ->
	Japp ++ "-" ++ version(Japp, Path) ++ ".zip".
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
get_additional_configs_test() ->
	{ok, CWD} = file:get_cwd(),
	Result = [filename:join([CWD, "test/conf_2", "projects/projects.config"]), filename:join([CWD, "test/conf_2", "projects/projects.config"])],
	?assertEqual([], get_additional_configs(filename:join([CWD, "test", "conf_1"]))),
	?assertEqual(Result, get_additional_configs(filename:join([CWD, "test", "conf_2"]))).

get_deps_config_test() ->
	{ok, CWD} = file:get_cwd(),

	?assertEqual([], get_deps_config(filename:join([CWD, "test", "conf_1"]))),
	?assertEqual([filename:join([CWD, "test/conf_2", "deps","jsx"])], get_deps_config(filename:join([CWD, "test", "conf_2"]))).

backup_test() ->
	backup(opm).
-endif.
