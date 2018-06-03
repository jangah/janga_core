%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 20.02.2014

-module(janga_config).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/janga_core.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([get_module_config/1, set_module_config/2]).
-export([get_value/2, get_value/3, get_values/2, get_level_values/3]).
-export([get_ports/1, get_port/1, get_env/2, get_env/3]).
-export([get_service_config/1, get_name/1, get_notify/0]).
-export([get_exometer/0, run_exometer/0]).
-export([get_name/0, get_repo_uri/0, get_repo_dir/0]).
-export([get_japps_dir/0]).
-export([get_env/0]).
-export([add_access_log/0, delete_access_log/0, add_perf_log/0, delete_perf_log/0]).
-export([get_core_version/0, get_japp_version/1]).

get_core_version() ->
  get_japp_version(janga_core).

get_japp_version(Japp) when is_list(Japp) ->
  get_japp_version(list_to_atom(Japp));
get_japp_version(Japp) when is_atom(Japp) ->
  case application:get_key(Japp, vsn) of
    {ok, Version} -> Version;
    undefined -> lager:error("can't read the version of ~p", [Japp]), []
  end.

add_access_log() ->
  ok = webmachine_log:add_handler(webmachine_access_log_handler, ["/tmp"]).  

delete_access_log() ->
  ok = webmachine_log:delete_handler(webmachine_access_log_handler).  

add_perf_log() ->
  ok = webmachine_log:add_handler(webmachine_perf_log_handler, ["/tmp"]).  

delete_perf_log() ->
  ok = webmachine_log:delete_handler(webmachine_perf_log_handler).  

get_env() ->
   case init:get_argument(janga_env) of
      error -> prod;
      {ok, [[Env]]} -> Env
    end.

get_module_config(Config) ->
    {driver, _Module, Module_config} = lists:keyfind(driver, 1, Config),
    Module_config.

set_module_config(Config, Module_config) ->
    {driver, Module, _Module_config} = lists:keyfind(driver, 1, Config),
    lists:keyreplace(driver, 1, Config, {driver, Module, Module_config}).

get_level_values(Level, Keys, List_of_tuples) ->
    List_of_tuples_1 = get_level(Level, List_of_tuples),
    get_values(Keys, List_of_tuples_1).

get_level([], List_of_tuples) when is_list(List_of_tuples)  ->
    List_of_tuples;
get_level([Key|Keys], List_of_tuples) when is_list(List_of_tuples)  ->
    get_level(Keys, get_value(Key, List_of_tuples)).

get_values(Keys, List_of_tuples) when is_list(List_of_tuples)  ->
    get_values(Keys, List_of_tuples, []). 

get_values([], _List_of_tuples, Acc) ->
    lists:reverse(Acc);
get_values([Key|Keys], List_of_tuples, Acc) when is_list(List_of_tuples) ->
    get_values(Keys, List_of_tuples, [get_value(Key, List_of_tuples)|Acc]).

get_value(Key, List_of_tuples) when is_list(List_of_tuples)  ->
    get_value(proplists:lookup(Key, List_of_tuples)).
get_value(Key, List_of_tuples, Default) when is_list(List_of_tuples)  ->
    case get_value(proplists:lookup(Key, List_of_tuples)) of
      none -> Default;
      Value -> Value
    end.

get_value({_K, _V1, V2}) ->
  V2;
get_value({_K, V}) ->
  V;
get_value(none) ->
  none.

get_port(Japp) ->
  case is_application_loaded(Japp, application:loaded_applications()) of
    true ->  get_env(Japp, port);
    false -> application:load(Japp),
             Port = get_env(Japp, port),
             application:unload(Japp),
             Port
  end.

get_ports(Japps) ->
  get_ports(Japps, []).

get_ports([], Acc) ->  
  Acc;
get_ports([Japp|Japps], Acc) ->
  get_ports(Japps, [{Japp, get_port(Japp)}|Acc]).

is_application_loaded(Japp, Apps) ->
  case lists:keyfind(Japp, 1, Apps) of
    false -> false;
    _ -> true
  end. 

get_repo_uri() ->
  get_env(janga_core, repo_uri).

get_repo_dir() ->
  get_env(janga_core, repo_dir).

get_japps_dir() ->
  get_env(janga_core, japps_dir).

get_name() ->
  get_env(janga_core, name).

run_exometer() ->
  Config = get_env(janga_core, exometer, []),
  case lists:keyfind(run, 1, Config) of
    false -> false;
    {run, Flag} -> Flag
  end. 

get_exometer() ->
  get_env(janga_core, exometer, []).

get_env(Application, Key) ->
  {ok, Value} = application:get_env(Application, Key),
  Value.

get_env(Application, Key, Default) ->
  application:get_env(Application, Key, Default).


get_name(Application) ->
  get_value(name, get_service_config(Application)).

get_service_config(Application) ->
  [{service, Name, Config}] = janga_config_handler:get_config(Application, ?SERVICE_CONFIG),
  [{name, Name}| Config].

get_notify() ->
  get_env(janga_core, notify).
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).


get_value_test() ->
    List_of_tuples = [{key1, "V1"}, {key2, "v2"}, {key3, [{sub3_k1, "v31"}, {sub3_k2, "v32"}]}, {key4, "v4"}],
    ?assertEqual([{sub3_k1,"v31"},{sub3_k2,"v32"}], get_value(key3, List_of_tuples)).

get_value_default_test() ->
    List_of_tuples = [{key1, "V1"}, {key2, "v2"}, {key3, [{sub3_k1, "v31"}, {sub3_k2, "v32"}]}, {key4, "v4"}],
    ?assertEqual(true, get_value(unknown, List_of_tuples, true)).

get_level_test() ->
    List_of_tuples = [{key1, "V1"}, {key2, "v2"}, {key3, [{sub3_k1, "v31"}, {sub3_k2, "v32"}]}, {key4, "v4"}],
    ?assertEqual([{sub3_k1, "v31"}, {sub3_k2, "v32"}], get_level([key3], List_of_tuples)).  

get_level_values_test() ->
    List_of_tuples = [{key1, "V1"}, {key2, "v2"}, {key3, [{sub3_k1, "v31"}, {sub3_k2, "v32"}]}, {key4, "v4"}],
    ?assertEqual(["v31"], get_level_values([key3], [sub3_k1], List_of_tuples)).

get_values_test() ->
    List_of_tuples = [{key1, "V1"}, {key2, "v2"}, {key3, "v3"}, {key4, "v4"}],
    ?assertEqual(["v2", "v4"], get_values([key2, key4], List_of_tuples)).

get_module_config_test() ->

    Config = 
     [{id, "0"},
      {activ, false},
     {driver, {fungi_driver,handle_msg},
        [{data,[{window,close}]},
         {config,
            [{rooms, [{"horst@raspberrypi",{"Büro", 3}},
                      {"horst@erwin", {"Wohnzimmer", 2}}]},                
            {hum_max,"60.0"},
            {temp_min,"16.0"},
            {temp_max,"21.0"}]}]}],

    Result = [{data,[{window,close}]},
         {config,
            [{rooms, [{"horst@raspberrypi",{"Büro", 3}},
                      {"horst@erwin", {"Wohnzimmer", 2}}]},                
            {hum_max,"60.0"},
            {temp_min,"16.0"},
            {temp_max,"21.0"}]}],

    ?assertEqual(Result, get_module_config(Config)).    
-endif.