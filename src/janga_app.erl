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
-module(janga_app).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start/1, stop/1, restart/1, autostart/0]).
-export([deploy/1, undeploy/1, update/1]).
-export([list_running/0, get_configs/0, get_messages/0, deployed_japps/0, ports/0]).
-export([version/1, check_version/1]).
-export([download/1, install/2]).

start(JApp) when is_list(JApp)->
	start(list_to_atom(JApp));
start(JApp) ->
	 case application:start(JApp) of
	 	ok -> janga_message:send([], system, [{start, finished}, {japp, JApp}]),  
	 		 lager:info("~p is started.", [JApp]);
	 	{error, Reason} -> lager:error("~p", [Reason]),
	 					   lager:error("Maybe ~p is not deployed. You have to deploy it first.", [JApp])
	 end.

stop(JApp)  when is_list(JApp)->
	stop(list_to_atom(JApp));
stop(JApp) ->	
	application:stop(JApp),
	application:unload(JApp),
	janga_message:send([], system, [{stop, finished}, {japp, JApp}]).

restart(JApp) ->
	stop(JApp),
	start(JApp),
	janga_message:send([], system, [{restart, finished}, {japp, JApp}]).

autostart() ->
	{ok, Japps} = application:get_env(janga_core, autostart),	
	[start(Japp)||Japp <- Japps].

update(JApp) when is_atom(JApp) ->
	update(atom_to_list(JApp));
update(JApp) ->
	janga_deploy:update(JApp).

deploy(JApp) when is_atom(JApp) ->
	deploy(atom_to_list(JApp));
deploy(JApp) ->
	janga_deploy:deploy(JApp).

undeploy(JApp) when is_atom(JApp) ->
	undeploy(atom_to_list(JApp));
undeploy(JApp) ->
	janga_deploy:undeploy(JApp).  

download(JApp) ->
	janga_deploy:download(JApp). 

install(JApp, ZipFile) ->
	janga_deploy:install(JApp, ZipFile).
	 
list_running() ->
	MatchHead = '_',
    Guard = [],
    Result = ['$$'],
    gproc:select(names, [{MatchHead, Guard, Result}]).

get_configs() ->
	{node(), [{Name, gen_server:call(Pid, {get_config})}|| [{n,l, Name}, Pid,undefined] <- list_running()]}.

get_messages() ->
	{node(),[{Name, gen_server:call(Pid, {get_allowed_messages})} || [{n,l, Name}, Pid,undefined] <- list_running()]}.

deployed_japps() ->
	[filename:basename(Dir) ||Dir <- filelib:wildcard("japps" ++ "/*"), filelib:is_dir(Dir)].

ports() ->
	janga_config:get_ports().

version(Japp) when is_list(Japp) ->
	version(list_to_atom(Japp));
version(Japp) when is_atom(Japp) ->
	L = application:loaded_applications(),	
	case lists:keysearch(Japp, 1, L) of
		false -> {false, "japp is not running."};
		{value,{Japp, _Desc, Version}} -> Version
	end.

check_version(Japp) when is_list(Japp) ->
	Path = janga_config:get_env(janga_core, repo_dir),
	case version(Japp) of
		{false, Reason} -> {false, Reason};
		V1 -> {ok, [{_A, _B, L}]} = file:consult(filename:join([Path, Japp, "ebin", Japp ++ ".app"])),
			  V2 = proplists:get_value(vsn, L),
			  case V1 =:= V2 of
			  	true -> {true, "versions are the same"};
			  	false -> {false, "versions are different, please update."}
			  end
	end.
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
