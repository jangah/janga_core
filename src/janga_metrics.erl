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
-define(INTERVAL_MINUTE, 1000 * 60).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([init/1, vm/0, counter/2, gauge/2, histogram/2, spiral/2]).

%% @doc Update a histogram statistic.
%% If an exometer entry is not already present, create a histogram and
%% subscribe to it with exometer_report_graphite.
-spec histogram(Name :: exometer:name(), Value :: number()) ->
    ok.
histogram(Name, Value) ->
    case exometer:update(Name, Value) of
        {error, not_found} ->
            exometer_admin:ensure(Name, histogram, [{module, exometer_histogram}]),
            exometer_report:subscribe(exometer_report_influxdb,
                                      Name, [mean, 50, 75, 95, 99],
                                      ?INTERVAL, [], true),
            exometer:update(Name, Value);
        ok ->
            ok
    end.

%% @doc Update a counter statistic.
%% If an exometer entry is not already present, create a counter and
%% subscribe to it with `exometer_report_graphite'.
-spec counter(Name :: exometer:name(), Value :: number()) ->
    ok.
counter(Name, Value) ->
    case exometer:update(Name, Value) of
        {error, not_found} ->
            exometer_admin:ensure(Name, counter, []),
            exometer_report:subscribe(exometer_report_influxdb,
                                      Name, [value],
                                      ?INTERVAL, [], true),
            exometer:update(Name, Value);
        ok ->
            ok
    end.


%% @doc Update a gauge statistic.
%% If an exometer entry is not already present, create a gauge and
%% subscribe to it with `exometer_report_graphite'.
-spec gauge(Name :: exometer:name(), Value :: number()) ->
    ok.
gauge(Name, Value) ->
    case exometer:update(Name, Value) of
        {error, not_found} ->
            exometer_admin:ensure(Name, gauge, []),
            exometer_report:subscribe(exometer_report_influxdb,
                                      Name, [value],
                                      ?INTERVAL, [], true),
            exometer:update(Name, Value);
        ok ->
            ok
    end.

%% @doc Update a spiral statistic.
%% If an exometer entry is not already present, create a gauge and
%% subscribe to it with `exometer_report_graphite'.
-spec spiral(Name :: exometer:name(), Value :: number()) ->
    ok.
spiral(Name, Value) ->
    case exometer:update(Name, Value) of
        {error, not_found} ->
            exometer_admin:ensure(Name, spiral, [{module, exometer_spiral}]),
            exometer_report:subscribe(exometer_report_influxdb,  Name, [count, one], ?INTERVAL_MINUTE, [], true),
            spiral(Name, Value);
        ok ->
            ok
    end.


init(ReportOptions) when is_list(ReportOptions) ->
    {ok, _Name} = inet:gethostname(),
%    ReportOptions = [{protocol, http},
%                     {host, <<"localhost">>},
%                     {port, 8086},
%                     {db, <<"exometer">>},
%                     {batch_window_size, ?INTERVAL},
%                     {tags, [{region, de}]}],
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
                                [input, output], ?INTERVAL, [], true),

  true = exometer_admin:set_default(['_'], cpu, [{module, exometer_cpu}]),
  ok = exometer:new([qstat, cpu], cpu, [{sample_interval, ?INTERVAL}]),
  ok = exometer_report:subscribe(exometer_report_influxdb, 
                                [qstat, cpu], 
                                [avg1, avg5, avg15], ?INTERVAL),
  
  case os:type() of
    {unix,darwin} -> lager:warning("on osx i can't measure the cpu temp, yet");
    {unix,linux}  -> true = exometer_admin:set_default(['_'], cpu_temp, [{module, jangah_cpu_temp}]),
                     ok = exometer:new([janga, cpu, temp], cpu_temp, [{sample_interval, ?INTERVAL}]),
                     ok = exometer_report:subscribe(exometer_report_influxdb, [janga, cpu, temp], 
                                                    [temp], ?INTERVAL)
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
