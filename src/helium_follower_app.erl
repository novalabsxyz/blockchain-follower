%%%-------------------------------------------------------------------
%% @doc helium-follower public API
%% @end
%%%-------------------------------------------------------------------

-module(helium_follower_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    GlobalOpts = application:get_env(rocksdb, global_opts, []),
    {ok, Cache} = rocksdb:new_cache(lru, 32 * 1024 * 1024),
    BBOpts = proplists:get_value(block_based_table_options, GlobalOpts, []),
    Opts1 = proplists:delete(block_based_table_options, GlobalOpts),
    Opts = Opts1 ++ [{block_based_table_options,
                      BBOpts ++ [{block_cache, Cache}]}],
    application:set_env(rocksdb, global_opts, Opts),

    case hf_sup:start_link() of
        {ok, Pid} ->
            hf_cli_registry:register_cli(),
            {ok, Pid};
        {error, Reason} -> {error, Reason}
    end.

stop(_State) ->
    ok.

%% internal functions
