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
-module(janga_http).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/janga_core.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([send_req/3, send_req/4,  send_req/5, send_req/6]).

send_req(Uri, Headers, Verb) ->
	send_req(Uri, Headers, Verb, [], []).

send_req(Uri, Headers, Verb, Body) ->
	send_req(Uri, Headers, Verb, Body, []).

send_req(Uri, Headers, Verb, Body, Options) ->
	send_req(Uri, Headers, Verb, Body, Options, 3000).

send_req(Uri, Headers, Verb, Body, Options, Timeout) ->
	case ibrowse:send_req(Uri, Headers, Verb, Body, Options, Timeout) of
		{error, Reason} -> lager:error("can't connect to : ~p, because of ~p", [Uri, Reason]),
							{error, "can't connect. For details, look at the log"};
		{ok, Status, _ResponseHeaders, _ResponseBody} = Any -> Any;
		Any -> Any
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

setup_test() ->
	application:start(ibrowse).

send_req_test() ->	
	?assertEqual({error, "can't connect. For details, look at the log"}, send_req("http://test", [], get)).

teardown_test() ->
	application:stop(ibrowse).
-endif.
