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

deploy(JApp) when is_atom(JApp) ->
	deploy(atom_to_list(JApp));
deploy(JApp) ->
	janga_message:send([], system, [{deploy, start}, {japp, JApp}]), 
	Config = get_config(JApp),
	Destination = janga_config:get_value(destination, Config),
	Source = janga_config:get_value(source, Config),
	Deploy_config = janga_config:get_value(deploy_config, Config),
	case file:make_dir(Destination) of
		{error,eexist} -> lager:error("the japp : ~p already exists.", [JApp]);
		{error,enoent} -> lager:error("the japp : ~p doesn't exists.", [JApp]);
		ok -> copy_dir(Source, Destination, JApp, ["messages.config", "service.config"]), 
			  add_path(Destination),			  
			  add_path_for_deps(Destination, Deploy_config)
	end,
	janga_message:send([], system, [{deploy, finished}, {japp, JApp}]), 
	lager:info("japp : ~p is deployed. Now you can start it.", [JApp]),
	ok.

undeploy(JApp) ->
	janga_message:send([], system, [{undeploy, start}, {japp, JApp}]), 
	Config = get_config(JApp),
	Destination = janga_config:get_value(destination, Config),
	janga_app:stop(JApp),	
	delete_dir(Destination),	
	janga_message:send([], system, [{undeploy, finished}, {japp, JApp}]), 
	lager:info("japp : ~p is undeployed.", [JApp]).

update('janga_core') ->
	update("janga_core");
update("janga_core") ->
	janga_message:send([], system, [{update, start}, {japp, "janga_core"}]),
	Path = janga_config:get_env(janga_core, janga_core_dir), 
	Destination = filename:join(filename:absname("deps") , "janga_core"),
	Source = filename:absname(filename:join(filename:absname(Path), "janga_core")),
	copy_dir(Source, Destination, "janga_core", []),	
	janga_message:send([], system, [{update, finished}, {japp, "janga_core"}]), 
	lager:info("japp : ~p is updated!", ["janga_core"]);

update(JApp) ->
	janga_message:send([], system, [{update, start}, {japp, JApp}]), 
	Config = get_config(JApp),
	Destination = janga_config:get_value(destination, Config),
	Source = janga_config:get_value(source, Config),
	Deploy_config = janga_config:get_value(deploy_config, Config),
	Additional_Config = janga_config:get_value(additional_config, Config),

	copy_dir(Source, Destination, JApp, [Additional_Config|["messages.config", "service.config"]]),			
	add_path_for_deps(Destination, Deploy_config),
	janga_message:send([], system, [{update, finished}, {japp, JApp}]), 
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
get_config(Japp) when is_list(Japp) ->
	get_config(list_to_atom(Japp));
get_config(Japp) when is_atom(Japp) ->
	Repo = janga_config:get_env(janga_core, repo_dir),
	Japps_dir = janga_config:get_env(janga_core, japps_dir),
	Destination = filename:join(filename:absname(Japps_dir) , Japp),
	Source = filename:absname(filename:join(filename:absname(Repo), Japp)),
	Configs = get_additional_configs(Destination),
	Deploy_config = get_deps_config(Destination),
	Config = [{repo_dir, Repo}, {destination, Destination}, {japps_dir, Japps_dir}, 
	{source, Source}, {additional_config, Configs}, {deploy_config, Deploy_config}],
	lager:info("config : ~p", [Config]),
	Config.
	
copy_dir(Source, Destination, JApp, Filter) ->
	Files = all_files_from_dir(Source),
	copy_files(Files, Destination, JApp, Filter).

copy_files([], _Destination, _JApp, _Filter) ->
	ok;
copy_files([File|Files], Destination, JApp, Filter) ->
	%lager:info("file to copy : ~p", [File]),
	case filelib:is_dir(File) of
		true -> create_dir(File, Destination, JApp);
		false -> case is_config_file(filename:join([Destination, extract_rest(File, JApp)]), Filter) of 
					false -> copy_file(File, Destination, JApp, Filter);
					true -> lager:info("we don't overwrite config file: ~p", [File])
				 end
%							Source_config = read_config(File),
%							Dest_config = read_config(filename:join([Destination, extract_rest(File, JApp)])),
%							Merged_config = merge_config(Dest_config, Source_config),
%							write_config(filename:join([Destination, extract_rest(File, JApp)]), Merged_config),
%							lager:info("merge_config : ~p", [Merged_config])
				 
	end,
	copy_files(Files, Destination, JApp, Filter).
	
write_config(Dest_config, false) ->
	lager:info("nothing to write yet");
write_config(Filename, Data) ->
	ok = file:write_file(Filename, io_lib:fwrite("~p.\n", [Data])).

read_config(Filename) ->
	{ok, [File]}=file:consult(Filename),
	File.

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
	lager:info("Filter : ~p", [Filter]),
	Files = filelib:wildcard(Destination ++ "/deps/*"),	
	[add_path(File)||File <- Files, filelib:is_dir(File), is_to_add(filename:join([Destination,"deps",File]), Filter)].

is_to_add(File, Filter) ->
	case lists:member(File, Filter) of
		true -> true;
		false -> delete_dir(File), false
	end.

get_deps_config(Path_for_japp) ->
	lager:info("Path_for_japp : ~p", [Path_for_japp]),
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
create_zip(File, Japp, Repo_path) when is_atom(Japp) ->
		create_zip(File, atom_to_list(Japp), Repo_path);
create_zip(File, Japp, Repo_path) when is_list(File) ->	
	Files = filelib:wildcard(Japp ++ "/**", Repo_path),
	{ok, ZipFile} = zip:zip(File, Files, [{cwd, Repo_path}]),
	ZipFile.

version(Japp, Path) when is_atom(Japp) ->
	version(atom_to_list(Japp), Path);
version(Japp, Path) ->	
	case file:consult(filename:join([Path, Japp, "ebin", Japp ++ ".app"])) of
		R = {ok, [{_A, _B, L}]} -> proplists:get_value(vsn, L);
		{error,enoent} -> "0"
	end.

create_zip_name(Japp, Path) when is_atom(Japp) ->
	create_zip_name(atom_to_list(Japp), Path);
create_zip_name(Japp, Path) ->
	Japp ++ "-" ++ version(Japp, Path) ++ ".zip".

merge_config([{"default", List}], [{"default", List}]) ->
	lager:info("don't do it now"),
	false;
merge_config([{service, Name, Old_config}], [{service, Name, New_config}]) ->
	MC_1 = merge_entries(Old_config, New_config),
	{driver, Module, Old_driver_config} = lists:keyfind(driver, 1, Old_config),
	{driver, Module, New_driver_config} = lists:keyfind(driver, 1, New_config),
	MC_2 = merge_entries(Old_driver_config, New_driver_config),	
	[{service, Name, lists:keyreplace(driver, 1, MC_1, {driver, Module, MC_2})}].

is_new_entry(Old_config, {_Key, _Value, _Value1}) ->
	false;
is_new_entry(Old_config, {Key, Value}) ->
	not proplists:is_defined(Key, Old_config).

merge_entries(Old_config, New_config) ->
	C = [Entry||Entry <- New_config, is_new_entry(Old_config, Entry)],
	lists:merge(C, Old_config).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

setup_application_env() ->
	application:set_env(janga_core, japps_dir, "testdata/japps"),
	application:set_env(janga_core, repo_dir, "testdata/repo").

get_config_test() ->
	{setup ,
	setup_application_env(),
	get_config(dashboard)
	}.

deploy_test() ->
	{setup ,
	setup_application_env(),
	deploy("dashboard")
	}.

update_test() ->
	{setup ,
	setup_application_env(),
	update("dashboard")
	}.

undeploy_test() ->
	{setup ,
	setup_application_env(),
	undeploy("dashboard")
	}.

get_deps_config_test() ->
	{ok, CWD} = file:get_cwd(),
	?assertEqual([], get_deps_config(filename:join([CWD, "testdata", "conf_1"]))),
	?assertEqual([filename:join([CWD, "testdata/conf_2", "deps","jsx"])], get_deps_config(filename:join([CWD, "testdata", "conf_2"]))).


backup_test() ->
	backup(opm).

merge_config_test() ->
	New = [{service,"opm",[
				{type,actor},
	 			{ets,false},
	 			{ui, true},
	 			{driver, opm_driver,[
							{default,"alarm1.mp3"},
							{emqtt, [
									{host, "horst"},
									{client_id, "jangah_client_opm"}]}
	 					 			]},
     			{activ,false},
     			{timer, 0},
     			{icon,"temp.png"},
     			{description,"opm"}]}],
    Old = [{service,"opm",[
				{type,actor},
	 			{ets,true},
	 			{ui, true},
	 			{driver, opm_driver,[
							{default,"alarm.mp3"}
							]},
     			{activ,false},
     			{timer, 0},
     			{icon,"temp.png"},
     			{description,"opm"}]}],
    Merged = [{service,"opm",[
				{type,actor},
	 			{ets,true},
	 			{ui, true},
	 			{driver, opm_driver,[							
	 						{default,"alarm.mp3"},
							{emqtt, [
									{host, "horst"},
									{client_id, "jangah_client_opm"}]}
	 					 			]},
     			{activ,false},
     			{timer, 0},
     			{icon,"temp.png"},
     			{description,"opm"}]}],

     ?assertEqual(Merged, merge_config(Old, New)).
-endif.
