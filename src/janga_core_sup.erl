-module(janga_core_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD_ARG_1(I, Type, Arg1), {Arg1, {I, start_link, [Arg1]}, temporary, 5000, Type, [I]}).
%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{one_for_one, 5, 10}, 
    	[        	
    	?CHILD(janga_ets_mgr, worker),    	
    	?CHILD(janga_actor_group, worker),
    	?CHILD(actor_group, worker),
    	?CHILD(janga_account_sup, supervisor),
    	?CHILD_ARG_1(janga_service_event, worker, "service_event_manager"),
        ?CHILD(janga_notifier, worker)
    	]
    	}}.

