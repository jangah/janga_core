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
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 

-module(janga_login).

-export([login/2, login/3]).
-export([check_session/2]).

-include_lib("webmachine/include/webmachine.hrl").

login(ReqData, Context) ->
    case is_peer_allowed(ReqData) of 
        true -> {true, ReqData, Context};
        false -> case wrq:get_req_header("authorization", ReqData) of
                    "Basic Og=="       -> {"Basic realm=Webmachine", ReqData, Context};
                    "Basic " ++ Base64 -> Str = base64:mime_decode_to_string(Base64),
                                            case string:tokens(Str, ":") of
                                                []                  -> {"Basic realm=Webmachine", ReqData, Context};
                                                [_D]                -> {"Basic realm=Webmachine", ReqData, Context};
                                                [Account, Password] -> case janga_account:is_valid_account(Account, Password) of
                                                                        true -> {true, ReqData, Context};
                                                                        false-> {"Basic realm=Webmachine", ReqData, Context};
                                                                        undefined -> {"Basic realm=Webmachine", ReqData, Context}
                                                                        end
                                            end;
                    _                 -> {"Basic realm=Webmachine", ReqData, Context}
                 end
    end.

login(Account, Password, ReqData) ->
    case is_peer_allowed(ReqData) of 
        true -> true;
        false -> janga_account:is_valid_account(Account, Password) 
    end.

is_peer_allowed(ReqData) ->
    Peer = wrq:peer(ReqData),
    {ok, Peers} = application:get_env(janga_core, peers),     
    lists:member(Peer, Peers).


check_session(ReqData, Chat) ->
    Session = wrq:get_cookie_value("id", ReqData),
    lager:info("Session : ~p", [Session]),
    check_cookie(Session, Chat).

check_cookie(undefined, _Chat) ->
    {error, "no session available!"};
check_cookie(Session, Chat) ->
    case mochiweb_session:check_session_cookie(list_to_binary(Session),
        calendar:time_to_seconds(erlang:time()), fun(A) -> A end, Chat) of
        {true, [_TSFuture, Account]} -> lager:info("Account : ~p", [Account]),
                                        {ok, Account};
        {false, [TSFuture, Account]} -> lager:error("Session is unvalid! : ~p,~p", [TSFuture, Account]),
                                        {error, "Error checking session!"}
    end.
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.