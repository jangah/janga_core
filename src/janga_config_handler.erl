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
-export([get_id/1, get_name/2, get_service_name/1]).
-export([write_config/3, set_active/2, get_icon/1]).


get_config(Application) ->
	[{service, Service, Config}] = get_config(Application, ?SERVICE_CONFIG),
	Msgs = get_config(Application, ?MESSAGES_CONFIG),
	Messages = get_messages_for_module(Msgs, get_id(Config)),
	{[{name, Service}|Config], Messages}.

get_messages_for_module(Messages, Id) ->
	case lists:keyfind(Id, 1, Messages) of 
		false ->  sets:new();
		{_M, MSGs} -> sets:from_list(MSGs)
	end. 

get_config(Application, Config_file) ->	
	case file:consult(filename:join([code:priv_dir(Application), "config", Config_file])) of
		{ok, [Config]} -> Config;
		{error, Reason} -> {error, Reason}
	end. 

get_icon(Application) ->
	Service = get_config(Application, ?SERVICE_CONFIG),
	[Icon] = janga_config:get_level_values([service], [icon], Service),
	case Icon of
		none -> "service.png";
		_Any -> Icon
	end.

set_active_([{service, Name, Config}], Status) ->
	[{service, Name, lists:keyreplace(activ, 1, Config, {activ, Status})}].

set_active(Application, Status) ->
	Config = get_config(Application, ?SERVICE_CONFIG),
	set_active(Application, Config, Status).

set_active(Application, Config, Status) when is_list(Config) ->
	Config_1 = set_active_(Config, Status),	
	write_config(Application, ?SERVICE_CONFIG, Config_1),
	Config_1.

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
	?assertEqual(false, is_type("a", "b")).	

set_active_test() ->
	set_active(janga_core, true).

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

	write_config(janga_core, "test.config", []),
	application:load(janga_core),
	write_config(janga_core, "test.config", Data),
	Config = janga_config_handler:get_config(janga_core, "test.config"), 	
	?assertEqual(Data, Config).	

get_icon_test() ->
	?assertEqual("temp.png", get_icon("janga_core")).

-endif.
