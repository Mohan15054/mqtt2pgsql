-module(mqtt2pgsql_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([start_pgsql_pool/1]).

-export([init/1]).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(Args) ->
    {ok, PgsqlPool} = start_pgsql_pool(Args),
    Children = [
        PgsqlPool
    ],
    RestartStrategy = {one_for_one, 1, 5},
    {ok, {RestartStrategy, Children}}.

start_pgsql_pool(Args) ->
    PoolSize = proplists:get_value(poolSize, Args),

    ChildSpec = poolboy:child_spec(mqtt2pgsql_psql_pool_worker, [
        {name, {local, mqtt2pgsql_psql_pool_worker}},
        {worker_module, mqtt2pgsql_psql_pool_worker},
        {size, PoolSize}
    ],Args),

    {ok, ChildSpec}.