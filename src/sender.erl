-module(sender).
-export([publish/1]).

-include_lib("amqp_client/include/amqp_client.hrl").

publish(Message) ->
    RabbitMQURL = get_rabbitmq_url(),
    RabbitMQUser = get_rabbitmq_user(),
    RabbitMQPass = get_rabbitmq_pass(),
    {ok, Connection} = amqp_connection:start(#amqp_params_network{
        host = RabbitMQURL,
        username = RabbitMQUser,
        password = RabbitMQPass
    }),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    Publish = #'basic.publish'{
        routing_key = <<"Goodbye">>
    },
    amqp_channel:call(Channel, Publish, #amqp_msg{payload = Message}),
    amqp_connection:close(Connection).

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