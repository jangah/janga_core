%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created :  
-module(janga_config_handler).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/janga_core.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([get_config/1, get_config/2]).
-export([is_active/1]).
-export([set_active/3]).
-export([get_id/1, get_name/2, get_service_name/1]).
-export([write_config/3]).


get_config(Application) ->
	[{service, Service, Config}] = get_config(Application, "service.config"),
	Msgs = get_config(Application, "messages.config"),
	Messages = get_messages_for_module(Msgs, get_id(Config)),
	{[{name, Service}|Config], Messages}.

get_messages_for_module(Messages, Id) ->
	case lists:keyfind(Id, 1, Messages) of 
		false ->  sets:new();
		{_M, MSGs} -> lager:info("...~p", [MSGs]),sets:from_list(MSGs)
	end. 

get_config(Application, Config_file) ->
	case file:consult(filename:join([code:priv_dir(Application),"config", Config_file])) of
		{ok, [Config]} -> Config;
		{error, Reason} -> {error, Reason}
	end. 

is_active_set({service, _Name, Config}) ->
	confi:get_value(activ, Config, false).

set_active(Config, Name, Status) when is_list(Config) ->	
	Config_1 = set_active(Config, Status),
	Config_new = lists:keyreplace(Name, 2, Config, Config_1),
	write_config(horst, ?SERVICE_CONFIG, Config_new),
	Config_new.

set_active({service, Name, Config}, Status) ->
	{service, Name, lists:keyreplace(activ, 1, Config, {activ, Status})}.


write_config(Application, Config_file, Data) ->
	ok = file:write_file(filename:join([code:priv_dir(Application), "config", Config_file]), io_lib:fwrite("~p.\n", [Data])).

get_id([]) ->
	"default";
get_id(Config) when is_list(Config) ->
	janga_config:get_value(id, Config, "default"). 

get_name([], []) ->
	[];		
get_name([], Optional) ->
	Optional;
get_name(Config, Optional) ->
	case janga_config:get_value(name, Config) of 
		undefined -> [];
		Name -> [{name, Name}|Optional]
	end.

get_service_name(Optional) ->
	case janga_config:get_value(name, Optional) of 
		undefined -> [];
		Name -> Name 
	end.	
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
is_active(true) ->
	true;
is_active(false) ->
	false;
is_active(List) when is_list(List) ->
	janga_config:get_value(activ, List, false). 

is_type(Type, Type) ->
	true;
is_type(Type, Type_1) ->
	false.

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

is_type_test() ->
	?assertEqual(true, is_type("a", "a")),
	?assertEqual(true, is_type("a", "b")).	

is_active_set_test() ->
	Config = {service, "Switches office",
		[{type, actor},
		{driver, transmitter_433_driver, [{"Ventilator",1},{"Licht",2}]},
		{activ, true},
		{description,"Switches in my office"}]},
	Config_1 = {thing, "Switches office",
		[{type, actor},
		{driver, transmitter_433_driver, [{"Ventilator",1},{"Licht",2}]},
		{activ, false},
		{description,"Switches in my office"}]},

	?assertEqual(true, is_active_set(Config)),
	?assertEqual(false, is_active_set(Config_1)).

set_active_test() ->
	Config = {serivce, "Switches office",
		[{type, actor},
		{driver, transmitter_433_driver, [{"Ventilator",1},{"Licht",2}]},
		{activ, true},
		{description,"Switches in my office"}]},

	Config_1 = set_active(Config, false),
	?assertEqual(false, is_active_set(Config_1)).	

set_active_1_test() ->
	Config = [{service, "Switches office",
		[{type, actor},
		{driver, transmitter_433_driver, [{"Ventilator",1},{"Licht",2}]},
		{activ, true},
		{description,"Switches in my office"}]},
		{thing, "Message_Logger",
		[{type, actor},
		{driver, transmitter_433_driver, [{data, []}]},
		{activ, true},
		{description,"Message Logger"}]}],

	Config_1 = [{service, "Switches office",
		[{type, actor},
		{driver, transmitter_433_driver, [{"Ventilator",1},{"Licht",2}]},
		{activ, true},
		{description,"Switches in my office"}]},
		{thing, "Message_Logger",
		[{type, actor},
		{driver, transmitter_433_driver, [{data, []}]},
		{activ, false},
		{description,"Message Logger"}]}],
	?assertEqual(Config_1, set_active(Config, "Message_Logger", false)).

write_config_test() ->
	Data = [{service, "Switches office",
		[{type, actor},
		{driver, transmitter_433_driver, [{"Ventilator",1},{"Licht",2}]},
		{activ, true},
		{description,"Switches in my office"}]},
		{service, "MMM",
		[{type, actor},
		{driver, transmitter_433_driver, [{"Ventilator",1},{"Licht",2}]},
		{activ, true},
		{description,"mm"}]}],

	write_config(horst, "test.config", []),
	application:load(horst),
	write_config(horst, "test.config", Data),
	Config = janga_config_handler:get_config(horst, "test.config"), 	
	?assertEqual(Data, Config).	

-endif.
