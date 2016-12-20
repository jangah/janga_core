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
-export([init/0, vm/0]).

init() ->
    {ok, _Name} = inet:gethostname(),
    ReportOptions = [{protocol, http},
                     {host, <<"192.168.178.48">>},
                     {port, 8086},
                     {db, <<"exometer">>},
                     {tags, [{region, de}]}],


    ok = exometer_report:add_reporter(exometer_report_influxdb, ReportOptions).


vm() ->
  ok = exometer:new([erlang, memory],
                    {function, erlang, memory, ['$dp'], value,
                    [total, processes, system, atom, binary, ets]}),

	ok = exometer_report:subscribe(exometer_report_influxdb,
								                [erlang, memory],
								                [total, processes, system, atom, binary, ets], 
								                ?INTERVAL, [], true),

  ok = exometer:new([erlang, system],
                    {function, erlang, system_info, ['$dp'], value,
                    [process_count, port_count]}),

  ok = exometer_report:subscribe(exometer_report_influxdb,
                                [erlang, system],
                                [process_count, port_count], ?INTERVAL,
                                [], true),

      % VM statistics.
    ok = exometer:new([erlang, statistics],
                      {function, erlang, statistics, ['$dp'], value,
                       [run_queue]}),
    ok = exometer_report:subscribe(exometer_report_influxdb,
                                   [erlang, statistics],
                                   [run_queue], ?INTERVAL, [], true),

    ok = exometer:new([erlang, gc],
                      {function, erlang, statistics, [garbage_collection],
                       match, {total_coll, rec_wrd, '_'}}),
    ok = exometer_report:subscribe(exometer_report_influxdb,
                                   [erlang, gc],
                                   [total_coll, rec_wrd], ?INTERVAL, [], true),

    ok = exometer:new([erlang, io],
                      {function, erlang, statistics, [io], match,
                       {{'_', input}, {'_', output}}}),
    
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
