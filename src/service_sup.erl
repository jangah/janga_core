-module(service_sup).

-behaviour(supervisor).

%% API
-export([start_link/4]).

%% Supervisor callbacks
-export([init/1]).
-export([create_name/1]).

%% Helper macro for declaring children of supervisor
-define(SUPNAME(Name, Arg1), list_to_atom(lists:concat([Name, Arg1]))).
-define(CHILD_ARG_3(I, Type, Arg1, Arg2, Arg3), {Arg1 , {I, start_link, [Arg1, Arg2, Arg3]}, permanent, 5000, Type, [I]}).
%% ===================================================================
%% API functions
%% ===================================================================
%%
create_name(Service) ->
    ?SUPNAME(Service, "_service_sup").

start_link(Name, Service, Config, Messages) ->
    supervisor:start_link({local, list_to_atom(Name)}, ?MODULE, [Service, Config, Messages]).

 % ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([Service, Config, Messages]) ->	
    RestartStrategy = {one_for_one, 3, 3600},
    {ok, {RestartStrategy, [
    						?CHILD_ARG_3(janga_core_service, worker, Service, Config, Messages)
    						]}}.