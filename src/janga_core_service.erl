%% Copyright 2010 Ulf Angermann
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 
%%% -------------------------------------------------------------------
-module(janga_core_service).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/janga_core.hrl").
%% --------------------------------------------------------------------
%% External exports

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/3]).
-export([get_type/1, get_driver/1, is_activ/1, get_timer/1, get_database/1, get_description/1]).
-export([get_state/1, set_state/2, get_start_time/1, get_name/1, get_icon/1]).
-export([save_data_to_ets/2, save_data_to_ets/3, get_table_id/1, get_model/1, set_value/2, get_value/1, get_value/2]).
-export([get_pid/1, where_is_message_from/1, get_allowed_messages/1, get_config/1]).
-export([stop/1]).
-export([send_time_based/5, send_message/2]).
-export([get_module_config/1, set_module_config/2]).
-export([update_config/1]).

%% ====================================================================
%% External functions
%% ====================================================================
update_config(App_name) when is_list(App_name)->
    gen_server:cast(?LOOKUP_PID(App_name), {update_config, App_name}).    

send_message(Name, Message) ->
    gen_server:cast(list_to_atom(Name), {send_message, Message}).

send_time_based(Time, Pid, Name, Optional, Payload) when is_pid(Pid) ->
    gen_server:cast(Pid, {send_time_based, Pid, Name, Time, Optional, Payload}).

get_value(Name) ->
    gen_server:call(?REGISTRY_NAME(Name), {get_value}).

get_value(Node, Name) when is_atom(Node) ->
    rpc:call(Node, janga_core_service, get_value, [Name]).

get_pid(Name) ->
    whereis(Name). 

set_value(Pid, Value) when is_pid(Pid) ->
    gen_server:cast(Pid, {set_value, Value}). 

get_name(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, {get_name});
get_name(undefined) ->
    undefined.

get_start_time(Name) ->
    gen_server:call(?REGISTRY_NAME(Name), {get_start_time}).

get_type(Name) ->
    try 
        gen_server:call(?REGISTRY_NAME(Name), {get_type}, 100)
    catch
        _:_ -> undefined
    end.

get_icon(Name) ->
    gen_server:call(?REGISTRY_NAME(Name), {get_icon}).

get_driver(Name) ->
	gen_server:call(?REGISTRY_NAME(Name), {get_driver}).

is_activ(Name) ->
	gen_server:call(?REGISTRY_NAME(Name), {is_activ}).

get_timer(Name) ->
	gen_server:call(?REGISTRY_NAME(Name), {get_timer}).

get_database(Name) ->
	gen_server:call(?REGISTRY_NAME(Name), {get_database}).

get_description(Name) ->
	gen_server:call(?REGISTRY_NAME(Name), {get_description}).

get_state(Name) ->
    gen_server:call(?REGISTRY_NAME(Name), {get_state}).

get_module_config(Name) ->
    gen_server:call(?REGISTRY_NAME(Name), {get_module_config}).

set_module_config(Config, Module_config) ->
    {driver, Driver, _Data} = lists:keyfind(driver, 1, Config), 
    lists:keyreplace(driver, 1, Config, {driver, Driver, Module_config}). 

get_model(Name) ->
    gen_server:call(?REGISTRY_NAME(Name), {get_model}).

set_state(Name, State) ->
    gen_server:call(?REGISTRY_NAME(Name), {set_state, State}).

get_allowed_messages(Name) ->
    gen_server:call(?REGISTRY_NAME(Name), {get_allowed_messages}).    

get_config(Name) ->
    gen_server:call(?REGISTRY_NAME(Name), {get_config}).    

stop(Name) ->
    gen_server:cast(?REGISTRY_NAME(Name), {stop}).

%% --------------------------------------------------------------------
%% record definitions   
%% --------------------------------------------------------------------
-record(state, {config, allowed_msgs, start_time, value, timed_msgs}).
%% ====================================================================
%% Server functions
%% ====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Service, Config, Messages) ->
    gen_server:start_link(?REGISTRY_NAME(Service), ?MODULE, [Service, Config, Messages], []).
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([_Service, Config, Messages]) ->    
    process_flag(trap_exit, true),
    {ok, #state{config = Config, allowed_msgs = Messages, start_time=0, value=undefined, timed_msgs=dict:new()}, 0}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({get_value}, _From, State=#state{value = Value}) ->
    {reply, Value, State};
handle_call({get_name}, _From, State=#state{config = Config}) ->
    {reply, janga_config:get_value(name, Config) , State};
handle_call({get_start_time}, _From, State=#state{start_time = Start_time}) ->
    {reply, Start_time, State};
handle_call({get_type}, _From, State=#state{config = Config}) ->
    {reply, janga_config:get_value(type, Config, undefined) , State};
handle_call({get_driver}, _From, State=#state{config = Config}) ->
	{driver, Module, Module_config} = lists:keyfind(driver, 1, Config),
    {reply, {Module, Module_config} , State};
handle_call({get_icon}, _From, State=#state{config = Config}) ->
    {reply, janga_config:get_value(icon, Config, "service.png") , State};
handle_call({get_config}, _From, State=#state{config = Config}) ->
    {reply, Config, State};
handle_call({is_activ}, _From, State=#state{config = Config}) ->
    {reply, janga_config:get_value(activ, Config) , State};
handle_call({get_timer}, _From, State=#state{config = Config}) ->
    {reply, janga_config:get_value(timer, Config, 0) , State};
handle_call({get_database}, _From, State=#state{config = Config}) ->
    {reply, janga_config:get_value(database, Config) , State};
handle_call({get_description}, _From, State=#state{config = Config}) ->
    {reply, janga_config:get_value(description, Config) , State};
handle_call({get_state}, _From, State) ->
    {reply, State, State};
handle_call({set_state, New_state}, _From, State) ->
    {reply, State, New_state};

handle_call({get_module_config}, _From, State=#state{config = Config}) ->
    case janga_config:get_value(ets, Config, false) of 
        true -> Table_Id = proplists:get_value(table_id, Config),
                {reply, ets:tab2list(Table_Id) , State};
        false -> {driver, _Module, Module_config} = lists:keyfind(driver, 1, Config),
                 {reply, Module_config, State}
    end;
handle_call({get_model}, _From, State=#state{config = Config}) ->    
    Reply = case janga_config:get_value(model_fun, Config, []) of
        [] -> [];
        {Model, Function} -> F=list_to_atom(Function), 
                             Fun = fun Model:F/0,
                             Fun()
    end,
    {reply, Reply, State};   

handle_call({get_allowed_messages}, _From, State=#state{allowed_msgs = Allowed_msgs}) ->     
    {reply, sets:to_list(Allowed_msgs), State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: save_data_to_ets/2
%% Description: Saves data to the ets of the service
%% Returns: true
%% --------------------------------------------------------------------
save_data_to_ets(Config, Value) ->
    save_data_to_ets(Config, data, Value).

save_data_to_ets(Config, Key, Value) ->
  Table_Id = janga_config:get_value(?TABLE, Config),
  ets:insert(Table_Id, [{Key, Value}]).

get_table_id(Config) ->
    janga_config:get_value(?TABLE, Config).
%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(die, State) ->
    exit(self(),kill),
    {noreply, State};
handle_cast({message, [Node ,Sensor, Id, Time, Optional, Payload]}, State=#state{allowed_msgs = Allowed_msgs, config = Config}) ->
    lager:debug("Message=~p ", [[Node ,Sensor, Id, Time, Optional, Payload]]),
    Config_1 = handle_msg([Node ,Sensor, Id, Time, Optional, Payload], Config, is_message_well_known({Node, Sensor, Id}, Allowed_msgs)),
    {noreply, State#state{config = Config_1}};

handle_cast({send_time_based, Pid, Name, Time, Optional, Payload}, State=#state{timed_msgs = Time_msgs}) ->
    case dict:find({Pid, Name}, Time_msgs) of 
        {ok, Timer_ref} -> erlang:cancel_timer(Timer_ref);                                                   
        error -> ok
    end,
    New_Timer_ref = erlang:send_after(Time, Pid, {send_after, Name, Optional, Payload}),
    {noreply, State#state{timed_msgs = dict:store({Pid, Name}, New_Timer_ref, Time_msgs)}};
handle_cast({set_value, Value}, State=#state{config = Config}) ->
    Name = janga_config:get_value(name, Config),
    notify(janga_service_event:exists(), Name, Value),
    {noreply, State#state{value=Value}};
handle_cast({stop}, State=#state{config = Config}) ->
    Name = janga_config:get_value(name, Config), 
    lager:info("stopping service : ~p ", [Name]),
     {stop, normal, State}; 
handle_cast({send_message, Message}, State=#state{config = Config}) ->
    {driver, Module, _Module_config} = lists:keyfind(driver, 1, Config),
    ?SEND_1(Module, Message),
    {noreply, State};
handle_cast({update_config, App_name}, State) ->
    lager:info("update config"),
    {Config, Messages} = janga_config_handler:get_config(App_name),
    {noreply, State#state{config = Config, allowed_msgs = Messages}};
handle_cast(_Msg, State) ->
    {noreply, State}.
%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(timeout, State=#state{config = Config}) ->    
	{driver, Driver, Module_config} = lists:keyfind(driver, 1, Config),
    Config_1 = ets_usage(janga_config:get_value(ets, Config, false), Config, Module_config),    
    Config_2 = driver_init(Driver, janga_config:get_value(init, Module_config, true), Config_1),
    true = gproc:reg({p, l, janga_config:get_value(type, Config)}, self()), 
	start_timer(janga_config:get_value(timer, Config_2, 0)),
    {noreply, State#state{start_time=now(), config = Config_2}};

handle_info({call_invoke}, State=#state{config = Config}) ->
    {driver, Module, Module_config} = lists:keyfind(driver, 1, Config),
    Config_1 = Module:janga_invoke(Config, Module_config),
    start_timer(janga_config:get_value(timer, Config, 0)),
    {noreply, State#state{config = Config_1}};

handle_info({gpio_interrupt, 0, Pin, Status}, State=#state{config = Config}) ->   
    lager:debug("gpio_interrupt for pin : ~p with status : ~p",[Pin, Status]),
    {driver, Module, Module_config} = lists:keyfind(driver, 1, Config),
    Config_1 = Module:janga_handle_msg({gpio_interrupt, 0, Pin, Status}, Config, Module_config),
    {noreply, State#state{config = Config_1}};

handle_info({external_interrupt, Application, _Data_type, Body} = Msg, State=#state{config = Config}) ->   
    lager:info("external_interrupt from Application : ~p with Body : ~p",[Application, Body]),
    {driver, Module, Module_config} = lists:keyfind(driver, 1, Config),
    Config_1 = Module:janga_handle_msg(Msg, Config, Module_config),
    {noreply, State#state{config = Config_1}};

handle_info({update_config, ?MESSAGES_CONFIG},  State=#state{config = Config, allowed_msgs = _Allowed_msgs}) ->
    {driver, Module, _Module_config} = lists:keyfind(driver, 1, Config), 
    Allowed_msgs_1 = node_config:get_messages_for_module(Module, janga_config_handler:get_id(Config)),    
    lager:info("update messages.config for service : ~p", [Module]),
    {noreply, State#state{allowed_msgs = Allowed_msgs_1}};

handle_info({'ETS-TRANSFER', TableId, Pid, _Data}, State=#state{config = Config}) ->
    lager:info("ETS Manager (~p) -> Serivce (~p) getting TableId: ~p~n", [Pid, self(), TableId]),
    Config_1 = case lists:keysearch(table_id, 1, Config) of 
        false -> [{table_id, TableId}| Config];
        {value, _Table} -> lists:keyreplace(table_id, 1, Config, {table_id, TableId})  
    end,
    {noreply, State#state{config = Config_1}};
 
handle_info({send_after, Name, Optional, Body}, State=#state{timed_msgs = Timed_msgs}) ->
    lager:info("now we send the message from : ~p with body : ~p ", [Name, Body]),
    janga_message:send([], Name, Optional, Body),
    {noreply, State#state{timed_msgs = dict:erase({self(), Name}, Timed_msgs)}};
    
handle_info({Port, Payload}, State=#state{config = Config}) when is_port(Port) ->
    lager:info("got a message from a port with payload: ~p ", [Payload]),
    {driver, Module, Module_config} = lists:keyfind(driver, 1, Config),
    Config_1 = Module:janga_handle_msg(Payload, Config, Module_config),        
    {noreply, State#state{config = Config_1}};

handle_info({'EXIT', _Port, normal}, State) ->
    {noreply, State};  

handle_info(Info, State) ->
    lager:error("~p got message : ~p that i don't understand. ~p", [?MODULE, Info, State]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, _State=#state{config = Config}) ->
    lager:info("Reason for termination : ~p",[Reason]),
    {driver, Module, _Module_config} = lists:keyfind(driver, 1, Config), 
    lager:info("stopping service of type : ~p", [Module]),
    Exports = janga_config:get_value(exports, Module:module_info(), []),
    case janga_config:get_value(janga_terminate, Exports) of 
        undefined -> lager:warning("there is no stop function in module : ~p", [Module]);
        1 -> driver_stop(Module, Config);
        _Any -> lager:warning("the stop function has too many arguments")
    end,
    delete_ets(Reason, Config),
    ok.
    
delete_ets(shutdown, Config) ->
    lager:info("delete_ets : ~p", [Config]),
    Table = proplists:get_value(?TABLE, Config),
    janga_ets_mgr:delete(Table);
delete_ets(Any, Config) ->
    ok.
%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
notify(true, _Name, []) ->
    %%lager:info(".... ~p", [Name]),
    false;
notify(true, Name, Value) ->
    janga_service_event:notify({Name, Value});
notify(false, _Name, _Value) ->
    false.

is_message_well_known({Node, Sensor, Id}, Allowed_msgs) ->    
    case  sets:is_element({Node, Sensor, Id}, Allowed_msgs) of 
        true -> true;
        false -> is_msgs_allowed({Node, Sensor, Id}, sets:to_list(Allowed_msgs))
    end.

is_msgs_allowed({Node, Sensor, Id}, [Check|Checks]) ->
    %%lager:info("is_allowed : ~p ~p ~p ~p", [Node, Sensor, Id, Checks]),
    case is_msgs_allowed({Node, Sensor, Id}, Check) of 
        true -> true;
        false -> is_msgs_allowed({Node, Sensor, Id}, Checks)
    end;

is_msgs_allowed({_Node, _Sensor, _Id}, {all, all, all}) ->
    true;
is_msgs_allowed({_Node, Sensor, _Id}, {all, Sensor, all}) ->
    true;
is_msgs_allowed({_Node, Sensor, _Id}, {all, Sensor1, all}) ->
    false;

is_msgs_allowed({_Node, _Sensor, Id}, {all, all, Id}) ->
    true;
is_msgs_allowed({_Node, _Sensor, Id}, {all, all, Id1}) ->
    false;

is_msgs_allowed({Node, _Sensor, _Id}, {Node, all, all}) ->
    true;
is_msgs_allowed({Node, _Sensor, _Id}, {Node1, all, all}) ->
    false;

is_msgs_allowed({Node, Sensor, _Id}, {Node, Sensor, all}) ->
    true;
is_msgs_allowed({Node, Sensor, _Id}, {Node1, Sensor1, all}) ->
    false;

is_msgs_allowed({Node, _Sensor, Id}, {Node, all, Id}) ->
    true;
is_msgs_allowed({Node, _Sensor, Id}, {Node1, all, Id1}) ->
    false;

is_msgs_allowed({_Node, Sensor, Id}, {all, Sensor, Id}) ->
    true;
is_msgs_allowed({_Node, Sensor, Id}, {all, Sensor1, Id1}) ->
    false;
% ------- local ------

is_msgs_allowed({Node, Sensor, Id}, {local, Sensor, Id}) ->
    is_msgs_allowed({Node, Sensor, Id}, {atom_to_binary(node(), utf8), Sensor, Id});
is_msgs_allowed({Node, Sensor, Id}, {local, all, all}) ->
    is_msgs_allowed({Node, Sensor, Id}, {atom_to_binary(node(), utf8), all, all});
is_msgs_allowed({Node, Sensor, Id}, {local, all, Id}) ->
    is_msgs_allowed({Node, Sensor, Id}, {atom_to_binary(node(), utf8), all, Id});
is_msgs_allowed({Node, Sensor, Id}, {local, Sensor, all}) ->
    is_msgs_allowed({Node, Sensor, Id}, {atom_to_binary(node(), utf8), Sensor, all});
is_msgs_allowed({Node, Sensor, Id}, {local, Sensor1, Id}) ->
    is_msgs_allowed({Node, Sensor, Id}, {atom_to_binary(node(), utf8), Sensor1, Id});
is_msgs_allowed({Node, Sensor, Id}, {local, Sensor, Id1}) ->
    is_msgs_allowed({Node, Sensor, Id}, {atom_to_binary(node(), utf8), Sensor, Id1});
is_msgs_allowed({Node, Sensor, Id}, {local, Sensor1, Id1}) ->
    is_msgs_allowed({Node, Sensor, Id}, {atom_to_binary(node(), utf8), Sensor1, Id1});

is_msgs_allowed({Node, Sensor, Id}, Allowed_msgs) ->
    false.

handle_msg([Node ,Sensor, Id, Time, Optional, Body], Config, true) ->
    %%lager:info("got message : ~p : ~p", [Time, Body]),
    {driver, Module, Module_config} = lists:keyfind(driver, 1, Config),
    Module:janga_handle_msg([Node ,Sensor, Id, Time, Optional, Body], Config, Module_config);

handle_msg([Node ,Sensor, Id, _Time, _Optional, _Body], Config, false) ->
    lager:debug("got message which i don't understand : ~p", [{Node, Sensor, Id}]),
    Config.

driver_init(Module, false, Config) ->
    lager:debug("don't init driver : ~p", [Module]),
    check_init(Module),
    Config;
driver_init(Module, true, Config) ->
    lager:debug("call init for driver : ~p", [Module]),    
    {ok, Config_1} = Module:janga_init(Config),
    Config_1.

driver_stop(Module, Config) ->
    lager:debug("call stop for driver : ~p", [Module]),    
    {ok, Config_1} = Module:janga_terminate(Config),
    Config_1.

start_timer(0) ->
	lager:info("timer for service  is set to 0");
start_timer(Time) ->
    erlang:send_after(Time, self(), {call_invoke}). 
    
ets_usage(true, Config, Module_config) ->
    Table_Id = create_ets(Config, Module_config),
    lager:info("owned from ets manager the table : ~p", [Table_Id]),
    [{table_id, Table_Id}|Config];
ets_usage(false, Config, _Module_config) ->
    Config.
    
create_ets(Config, Module_config) ->
    Name = janga_config:get_value(name, Config),
    Id = janga_config_handler:get_id(Config),
    janga_ets_mgr:init_table(self(), list_to_atom(Name ++ "_" ++ Id), Module_config).  

check_init(Module) ->
    Exports = janga_config:get_value(exports, Module:module_info(), []),
    case janga_config:get_value(janga_init, Exports) of 
        undefined -> true;
        1 -> lager:warning("there is a init function in the module '~p', but it is false or not available in the config", [Module]),
             false;
        _Any -> lager:warning("the init function has too many arguments"),
                false
    end.
where_is_message_from([Node, _Driver, _Id, Optional, _Payload]) ->    
    case janga_config_handler:get_service_name(Optional) of 
        [] -> [];
        Name -> rpc:call(binary_to_existing_atom(Node, utf8), erlang, whereis, [Name])
    end.
%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
is_message_well_known_test() ->
    Allowed_msgs_1 = sets:from_list([{<<"horst@raspberrypi">>,<<"hc_sr501_driver">>,<<"0">>}, {all, all, all}]),
    Allowed_msgs_2 = sets:from_list([{all, all, all}]),
    Allowed_msgs_3 = sets:from_list([{all, all, <<"0">>}]),
    ?assertEqual(true, is_message_well_known({<<"horst@raspberrypi">>,<<"hc_sr501_driver">>,<<"0">>}, Allowed_msgs_1)),
    ?assertEqual(true, is_message_well_known({<<"test@raspberrypi">>,<<"hc_sr501_driver">>,<<"0">>}, Allowed_msgs_2)),
    ?assertEqual(true, is_message_well_known({<<"test@raspberrypi">>,<<"hc_sr501_driver">>,<<"0">>}, Allowed_msgs_3)).

is_msgs_allowed_test() ->
    Allowed_msgs_1 = [{all, <<"hc_sr501_driver">>, all}],
    ?assertEqual(true, is_msgs_allowed({<<"horst@raspberrypi">>,<<"hc_sr501_driver">>,<<"0">>}, Allowed_msgs_1)),
    ?assertEqual(false, is_msgs_allowed({<<"horst@raspberrypi">>,<<"cube_driver">>,<<"0">>}, Allowed_msgs_1)),
    Allowed_msgs_2 = [{local, <<"hc_sr501_driver">>, all}],
    ?assertEqual(true, is_msgs_allowed({atom_to_binary(node(),utf8),<<"hc_sr501_driver">>,<<"0">>}, Allowed_msgs_1)).

-endif.