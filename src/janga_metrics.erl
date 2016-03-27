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
-module(janga_metrics).

-define(INTERVAL, 5000).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([vm/0]).

vm() ->
	ok = exometer_report:subscribe(exometer_report_influxdb,
								[erlang, memory],
								[total, processes, system, atom, binary, ets], 
								?INTERVAL, [], true),


	ok = exometer_report:subscribe(exometer_report_influxdb,
                                   [erlang, memory],
                                   [total, processes, system, atom, binary,
                                    ets], ?INTERVAL, [], true),


    ok = exometer_report:subscribe(exometer_report_influxdb,
                                   [erlang, system],
                                   [process_count, port_count], ?INTERVAL,
                                   [], true),

    % VM statistics.
    ok = exometer_report:subscribe(exometer_report_influxdb,
                                   [erlang, statistics],
                                   [run_queue], ?INTERVAL, [], true),

    ok = exometer_report:subscribe(exometer_report_influxdb,
                                   [erlang, gc],
                                   [total_coll, rec_wrd], ?INTERVAL, [], true),

    ok = exometer_report:subscribe(exometer_report_influxdb,
                                   [erlang, io],
                                   [input, output], ?INTERVAL, [], true).

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
