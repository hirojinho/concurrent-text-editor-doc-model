-module(receiver).

-export([start/0]).
-export([connect/0, loop/1]).

-include_lib("amqp_client/include/amqp_client.hrl").

start() ->
    Pid = spawn(?MODULE, connect, []),
    {ok, Pid}.

connect() ->
    RabbitMQURL = get_rabbitmq_url(),
    RabbitMQUser = get_rabbitmq_user(),
    RabbitMQPass = get_rabbitmq_pass(),
    RabbitMQPort = get_rabbitmq_port(),
    io:format("Connecting with user: ~p and pass: ~p~n", [RabbitMQUser, RabbitMQPass]),
    % Open a connection to the RabbitMQ server
    {ok, Connection} = amqp_connection:start(#amqp_params_network{
        host = RabbitMQURL,
        heartbeat = 30,
        username = RabbitMQUser,
        password = RabbitMQPass,
        port = RabbitMQPort
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

get_rabbitmq_url() ->
    case os:getenv("RABBITMQ_URL") of
        false -> io:format("RABBITMQ_URL not set, using default: localhost~n"), "localhost";
        RabbitMQURL -> RabbitMQURL
    end.

get_rabbitmq_user() ->
    case os:getenv("RABBITMQ_DEFAULT_USER") of
        false -> io:format("RABBITMQ_DEFAULT_USER not set, using default: guest~n"), <<"guest">>;
        RabbitMQUser -> RabbitMQUser
    end.

get_rabbitmq_pass() ->
    case os:getenv("RABBITMQ_DEFAULT_PASS") of
        false -> io:format("RABBITMQ_DEFAULT_PASS not set, using default: guest~n"), <<"guest">>;
        RabbitMQPass -> RabbitMQPass
    end.

get_rabbitmq_port() ->
    case os:getenv("RABBITMQ_PORT") of
        false -> io:format("RABBITMQ_PORT not set, using default: 5672~n"), 5672;
        RabbitMQPort -> list_to_integer(RabbitMQPort)
    end.
