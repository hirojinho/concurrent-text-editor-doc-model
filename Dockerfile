# doc_model/Dockerfile
FROM erlang:26

WORKDIR /app
COPY . .
CMD ["rebar3", "shell"]
