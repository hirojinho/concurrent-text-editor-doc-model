-module(doc_queue).

-export([enqueue/2]).
-export([process_queue/2]).

% Enqueue a change for the document's queue (spawn a new queue if it doesn't exist )
enqueue(DocId, Change) ->
    io:format("Enqueuing change for docId ~p~n", [DocId]),
    case doc_registry:lookup(DocId) of
        {error, not_found} ->
            io:format("DocId ~p not found in doc_registry~n", [DocId]),
            % The doc does not exists, create a new one
            doc:start(DocId),
            QueuePid = spawn(?MODULE, process_queue, [DocId, [Change]]),
            doc_registry:update_queue(DocId, QueuePid),
            QueuePid ! {enqueue, Change};
        {_, {_, undefined}} ->
            io:format("Queue for docId ~p is not active, starting a new one~n", [DocId]),
            % Queue is not active, start a new one with the change
            QueuePid = spawn(?MODULE, process_queue, [DocId, [Change]]),
            % Update the registry to register the new queue
            doc_registry:update_queue(DocId, QueuePid),
            QueuePid ! {enqueue, Change};
        {_, {_, QueuePid}} ->
            io:format("Queue for docId ~p is active, enqueuing the change~n", [DocId]),
            % Queue exists and is active, enqueue the change
            QueuePid ! {enqueue, Change};
        _Other ->
            io:format("Unexpected error in doc_queue:enqueue~n")
    end.

% Process the queue and terminate when done
process_queue(DocId, Queue) ->
    io:format("Processing queue for docId ~p~n", [DocId]),
    receive
        {enqueue, Change} ->
            % Add the change to the queue
            UpdateQueue = Queue ++ [Change],
            self() ! {process_next},
            process_queue(DocId, UpdateQueue);
        {process_next} when Queue =/= [] ->
            % Get the next change from the queue
            [NextChange | RestQueue] = Queue,
            % Lookup the document actor and send the change to it
            case doc_registry:lookup(DocId) of
                {ok, {ActorPid, _}} ->
                    ActorPid ! {newDoc, NextChange},
                    % Process the next change after current one is done
                    self() ! {process_next},
                    process_queue(DocId, RestQueue);
                {error, not_found} ->
                    io:format("Actor for document ~p not found during processing~n", [DocId]),
                    ok
            end;
        {process_next} ->
            % Queue is empty, terminate the process and publish the final document
            doc_registry:unregister_queue(DocId),
            case doc_registry:lookup(DocId) of
                {ok, {ActorPid, _}} ->
                    ActorPid ! {get_doc, self()},
                    receive
                        {doc, FinalDoc} ->
                            io:format("Final document: ~p~n", [FinalDoc]),
                            sender:publish(FinalDoc),
                            ok
                    end;
                undefined ->
                    io:format("Actor for document ~p not found during finalization~n", [DocId]),
                    ok
            end,
            ok;
        stop ->
            % Stop the queue process
            io:format("Stopping queue for document ~p~n", [DocId]),
            ok;
        Other ->
            io:format("Received unexpected message: ~p. Current state: ~p~n", [Other, Queue]),
            % If no new change or process message, continue the loop
            process_queue(DocId, Queue)
    after 1000 -> % Timeout for idle termination
            io:format("No messages received, terminating queue for document ~p~n", [DocId]),
            doc_registry:unregister_queue(DocId),
            ok
    end.
