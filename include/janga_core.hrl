
-define(MESSAGES_CONFIG, "messages.config").
-define(SERVICE_CONFIG, "service.config").

-define(APPLICATION, core).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(SERVICE(Name, Config), {Name, {janga_core_service, start_link, [Config]}, transient, 5000, worker, [janga_core_service]}).
-define(SEND(Body), janga_message:send(Config, ?MODULE, Body)).
-define(SEND(Module, Body), janga_message:send([], Module, Body)).
-define(SEND_1(Module, Body), janga_message:send(Config, Module, Body)).

-define(TABLE, table_id).

-define(SYSTEM, 'system').
-define(ON, "on").
-define(OFF, "off").
-define(RISING, "on").
-define(FALLING, "off").
-define(STARTED, "started").
-define(STOPPED, "stopped").
-define(MAX_QUEUE_LENGTH, 30).

-define(LOOKUP_ACTORS, gproc:lookup_values({p, l, actor})).
-define(REGISTRY_NAME(Name),{via, gproc, {n,l, Name}}).

-record(message, {
	node :: binary(),
	thing :: binary(),
	id :: binary(),
	time :: binary(),
	optional :: list(),
	payload :: list()
	}).

-record(counter, {key, value}).



