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

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([deploy/1, undeploy/1, update/1]).
-export([add_path_for_deps/2]).

deploy(JApp) ->
	Destination = filename:join(filename:absname("japps") , JApp),
	Source = filename:absname(filename:join(filename:absname("../japps"), JApp)),
	case file:make_dir(Destination) of
		{error,eexist} -> lager:warning("the japp : ~p already exists.", [JApp]);
		ok -> copy_dir(Source, Destination, JApp), 
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
	Destination = filename:join(filename:absname("japps") , JApp),
	Source = filename:absname(filename:join(filename:absname("../japps"), JApp)),
	copy_dir(Source, Destination, JApp),
	lager:info("japp : ~p is updated", [JApp]).

%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
copy_dir(Source, Destination, JApp) ->
	Files = all_files_from_dir(Source),
	copy_files(Files, Destination, JApp).

copy_files([], _Destination, _JApp) ->
	ok;
copy_files([File|Files], Destination, JApp) ->
	%%lager:info("file to copy : ~p", [File]),
	case filelib:is_dir(File) of
		true -> create_dir(File, Destination, JApp);
		false -> copy_file(File, Destination, JApp)
	end,
	copy_files(Files, Destination, JApp).

create_dir(Dir, Destination, JApp) ->
	%%lager:info("make dir : ~p", [filename:join([Destination, extract_rest(Dir, JApp)])]),
	file:make_dir(filename:join([Destination, extract_rest(Dir, JApp)])).

copy_file(File, Destination, JApp) ->
	%%lager:info("copy_file : ~p", [filename:join([Destination, extract_rest(File, JApp)])]),
	{ok, _BytesCopied} = file:copy(File, filename:join([Destination, extract_rest(File, JApp)])).

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
	lager:info("path : ~p", [Path_for_japp]),
	case file:consult(Path_for_japp ++ "/deploy.config") of
		{ok, Deps} -> create_path_for_deps(Path_for_japp, Deps);
		_ -> []
	end.

create_path_for_deps(_Path_for_japp, []) ->
	[];
create_path_for_deps(Path_for_japp, Deps) ->
	[filename:join([Path_for_japp,"deps" ,Dep])||Dep <- Deps].

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
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
all_files_from_dir_test() ->
	all_files_from_dir(".").
-endif.
