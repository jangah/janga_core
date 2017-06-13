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
-module(jangah_user_default).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([check_account/2]).

check_account(Account, Password) ->
	check_account(get_accounts(), Account, Password).
	
check_account(Accounts, Account, Password) ->
	case lists:keyfind(Account, 1, Accounts) of 
		{Account, Password} -> true;
						  _ -> false
	end.
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
get_accounts() ->
	{ok, Accounts} = file:consult(filename:join([code:priv_dir(janga_core), "config", "accounts.conf"])),
	Accounts.
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
