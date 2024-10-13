-module(doc_registry).

-behaviour(gen_server).

-export([start_link/0, stop/0, register/2, lookup/1, unregister_queue/1, update_queue/2]).
-export([init/1, handle_call/3, handle_cast/2]).

% Store document actors in a map
-record(state, {actors = #{}}).

start_link() ->
    {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    {ok, Pid}.

stop() ->
    gen_server:call(?MODULE, stop).

% Register a document actor
register(DocId, ActorPid) ->
    gen_server:call(?MODULE, {register, DocId, {ActorPid, undefined}}).

% Lookup a document actor by ID
lookup(DocId) ->
    gen_server:call(?MODULE, {lookup, DocId}).

unregister_queue(DocId) ->
    gen_server:call(?MODULE, {unregister_queue, DocId}).

update_queue(DocId, QueuePid) ->
    io:format("Updating queue for docId ~p to ~p~n", [DocId, QueuePid]),
    gen_server:call(?MODULE, {update_queue, DocId, QueuePid}).

init([]) ->
    io:format("Doc registry started with pid ~p~n", [self()]),
    {ok, #state{}}.

handle_call({register, DocId, {ActorPid, QueuePid}}, _From, State) ->
    % Register the document actor
    NewActors = maps:put(DocId, {ActorPid, QueuePid}, State#state.actors),
    {reply, ok, State#state{actors = NewActors}};

handle_call({lookup, DocId}, _From, State) ->
    case maps:get(DocId, State#state.actors, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        {ActorPid, QueuePid} ->
            {reply, {ok, {ActorPid, QueuePid}}, State};
        _Other ->
            {reply, {error, not_found}, State}
    end;

handle_call({unregister_queue, DocId}, _From, State) ->
    % Unregister the queue by setting the queue pid to undefined
    case maps:get(DocId, State#state.actors) of
        {ActorsPid, _} ->
            NewActors = maps:put(DocId, {ActorsPid, undefined}, State#state.actors),
            {reply, ok, State#state{actors = NewActors}};
        undefined ->
            {reply, ok, State}
    end;

handle_call({update_queue, DocId, QueuePid}, _From, State) ->
    % Update the queue pid for the document
    case maps:get(DocId, State#state.actors) of
        {ActorsPid, _OldQueuePid} ->
            NewActors = maps:put(DocId, {ActorsPid, QueuePid}, State#state.actors),
            {reply, ok, State#state{actors = NewActors}};
        undefined ->
            {reply, {error, not_found}, State}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_, State) ->
    io:format("Received unexpected cast message~n"),
    {noreply, State}.
