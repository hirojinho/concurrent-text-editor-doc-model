%%%-------------------------------------------------------------------
%% @doc actor top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(actor_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    % Start the doc_registry and receiver processes using ChildSpecs
    ChildSpecs = [
        {receiver, {receiver, start, []}, permanent, 5000, worker, [receiver]},
        {doc_registry, {doc_registry, start_link, []}, permanent, 5000, worker, [doc_registry]}
    ],

    % Log child specs for debugging
    lists:foreach(fun({Name, Start, _, _, _, _}) -> 
        io:format("Starting child process: ~p, Start: ~p~n", [Name, Start])
    end, ChildSpecs),

    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },

    {ok, {SupFlags, ChildSpecs}}.
    

%% internal functions
