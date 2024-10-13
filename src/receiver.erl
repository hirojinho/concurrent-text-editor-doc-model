-module(receiver).

-export([start/0]).
-export([connect/0, loop/1]).

-include_lib("amqp_client/include/amqp_client.hrl").

start() ->
    Pid = spawn(?MODULE, connect, []),
    {ok, Pid}.

connect() ->
    % Open a connection to the RabbitMQ server
    {ok, Connection} = amqp_connection:start(#amqp_params_network{
        host = "localhost",
        heartbeat = 30
    }),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    
    amqp_channel:call(Channel, #'queue.declare'{
        queue = <<"Hello">>
    }),
    io:format(" [*] Waiting for messages. To exit press CTRL+C~n"),

    Method = #'basic.consume'{
        queue = <<"Hello">>,
        no_ack = true
    },
    amqp_channel:subscribe(Channel, Method, self()),
    loop(Channel).

loop(Channel) ->
    receive
        #'basic.consume_ok'{} ->
            io:format(" [x] Saw basic.consume_ok~n"),
            loop(Channel);
        {#'basic.deliver'{
            delivery_tag = _Tag,
            redelivered = _Redelivered,
            exchange = _Exchange,
            routing_key = _RoutingKey
        }, #amqp_msg{
            payload = Body
        }} ->
            io:format(" [x] Received ~p~n", [Body]),
            % Assuming body is in the format {DocId, Change}
            case jsx:decode(Body, [return_maps]) of
                #{<<"doc_id">> := DocId, <<"change">> := Change} ->
                    % Assuming doc_queue is a module that handles enqueuing changes
                    doc_queue:enqueue(DocId, Change),
                    io:format("Received message: DocId = ~p, Change = ~p~n", [DocId, Change]),
                    loop(Channel);
                _Other ->
                    io:format("Received unexpected message format~n"),
                    loop(Channel)
            end
    end.