%% Copyright (c) 2013 Ulf Angermann
%% See MIT-LICENSE for licensing information.

%%% -------------------------------------------------------------------
%%% Author  : Ulf Angermann uaforum1@googlemail.com
%%% Description :
%%%
%%% Created : 

-module(janga_message).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([create_message/5]).
-export([send/3, send/4]).
-export([send_message/1, send_message/2]).
-export([send_messages/2]).
-export([send_after/5]).
-export([generate_messages/1, generate_messages/2]).
%% --------------------------------------------------------------------
%% record definitions
%% --------------------------------------------------------------------
create_message(Config, Module, Body) when is_list(Module) ->
	create_message(Config, list_to_atom(Module), Body);
create_message(Config, Module, Body) ->
	create_message(node(), Module, janga_config_handler:get_id(Config), janga_config_handler:get_name(Config, []), Body).

create_message(Config, Module, Optional, Body) ->
	create_message(node(), Module, janga_config_handler:get_id(Config), janga_config_handler:get_name(Config, Optional), Body).

create_message(Node, Module, Id, Optional, Body) ->
	create_message(Node, Module, Id, date:get_date_seconds(), Optional, Body).

create_message(Node, Module, Id, Time, Optional,Body) when is_list(Module) ->
	create_message(Node, list_to_atom(Module), Id, Time, Optional, Body);

create_message(Node, Module, Id, Time, Optional, Body) ->
    [atom_to_binary(Node, utf8), atom_to_binary(Module, utf8), list_to_binary(Id), list_to_binary(integer_to_list(Time)), Optional, Body].

%% doc sends a list of messages
-spec send_messages(_Config, [node()]) -> ok.
send_messages(_Config, []) ->
	ok;
send_messages(Config, [{Name, Message}|Messages]) when is_list(Messages) ->
	send(Config, Name, Message),
	send_messages(Config, Messages).

send(Config, Module, Body) ->	
	send_message(create_message(Config, Module, Body)).

send(Config, Module, Optional, Body) ->	
	send_message(create_message(Config, Module, Optional, Body)).

send_message(Message) ->
	send_message([node()|nodes()], Message).

send_message(Node, Message) when is_atom(Node) ->
	send_message([Node], Message);

send_message(Nodes, Message) when is_list(Nodes) ->
	send_message(Nodes, 'janga_actor_group', Message).    

send_message(Nodes, Target, Message) ->
	%janga_metrics:spiral([jangah, send, message], 1),
	[rpc:cast(Node_1, Target, 'broadcast', [Message]) || Node_1 <- Nodes],
	true.

send_after(Pid, Name, Time, Optional, Body) ->
	janga_core_service:send_time_based(Time, Pid, Name, Optional, Body).    
%%
%% doc generates messages and send them. !!! only for testing !!!
%%
generate_messages(Loop_count) ->
	timer:tc(janga_message, generate_messages, [Loop_count, 0]).
generate_messages(Counter, Counter) ->
	lager:info("finished generate_messages. ~p were generated.", [Counter]);
generate_messages(Counter, Value) ->
	Msg = create_message([], ?MODULE, {test_message, Value}),
	send_message([node()|nodes()], Msg),
	generate_messages(Counter, Value + 1).

%% --------------------------------------------------------------------
%%% Test functions
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).


create_message_test() ->
    ?assertEqual([<<"horst@notebook">>,<<"hc_sr501_sensor">>,<<"1">>, <<"63540684780">>,[],"FALLING"], create_message('horst@notebook', "hc_sr501_sensor", "1", 63540684780, [], "FALLING")).

-endif.