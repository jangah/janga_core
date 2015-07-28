-module(janga_core_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
	start_mnesia(),
    janga_core_sup:start_link().    

stop(_State) ->
	mnesia:stop(),    
    ok.

start_mnesia() ->
	lager:info("create the schema for the database"),
	stopped = mnesia:stop(),    
    mnesia:create_schema([node()]),
    ok = mnesia:start(),
	lager:info("schema is created and the database is running.").