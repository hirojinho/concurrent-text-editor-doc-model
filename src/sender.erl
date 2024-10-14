-module(sender).
-export([publish/1]).

-include_lib("amqp_client/include/amqp_client.hrl").

publish(Message) ->
    RabbitMQURL = os:getenv("RABBITMQ_URL"),
    {ok, Connection} = amqp_connection:start(#amqp_params_network{host = RabbitMQURL}),
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