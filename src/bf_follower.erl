-module(bf_follower).

-callback init() -> {ok, State::any()} | {error, term()}.
-callback load_chain(blockchain:blockchain(), State::any()) -> {ok, NewState::any()}.
-callback load(Hash::binary(),
               blockchain:block(),
               Sync::boolean(),
               blockchain_ledger_v1:ledger(),
               State::any()) -> {ok, NewState::any()}.
-callback terminate(State::any()) -> ok.

-optional_callbacks([terminate/1]).

-behaviour(gen_server).

%% API
-export([start_link/0,
         get_height/0, put_height/1,
         get_cache/1, put_cache/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(CACHE, bf_cache).
-define(CACHE_HEIGHT, bf_height).

-record(state,
        {
         chain=undefined :: undefined | blockchain:blockchain(),
         followers :: [{Module::atom(), State::any()}]
        }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_height() ->
    get_cache(?CACHE_HEIGHT, 2).

put_height(Height) ->
    put_cache({?CACHE_HEIGHT, Height}).

get_cache(Key) ->
    ets:lookup(?CACHE, Key).

get_cache(Key, N) ->
    ets:lookup_element(?CACHE, Key, N).

put_cache(Entry) ->
    ets:insert(?CACHE, [Entry]).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ok = register_blockchain_event_handler(self()),
    ets:new(?CACHE, [public, named_table, {read_concurrency, true}]),
    BaseDir = application:get_env(blockchain, base_dir, "data"),
    FollowerMods = application:get_env(blockchain_follower, follower_modules, []),
    Followers = lists:map(fun(M) ->
                                  {ok, S} = M:init(),
                                  {M, S}
                            end, FollowerMods),
    blockchain_worker:load(BaseDir, "update"),

    {ok, #state{followers=Followers}}.

handle_call(_Request, _From, State) ->
    lager:warning("unexpected call ~p from ~p", [_Request, _From]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    lager:warning("unexpected cast ~p", [_Msg]),
    {noreply, State}.

handle_info({blockchain_event, From, {new_chain, Chain}}, State=#state{followers=Followers}) ->
    Followers = lists:map(fun({M, S}) ->
                                  {ok, NS} = M:load_chain(Chain, S),
                                  {M, NS}
                          end, State#state.followers),
    acknowledge_blockchain_event(From),
    {noreply, State#state{chain=Chain, followers=Followers}};

handle_info({blockchain_event, From, {add_block, Hash, Sync, Ledger}},
            State=#state{chain=Chain, followers=Followers0}) ->

    {ok, Block} = blockchain:get_block(Hash, Chain),
    Height = get_height(),
    BlockHeight = blockchain_block:height(Block),
    LedgerRequired = application:get_env(blockchain_follower, ledger_required, true),

    Followers1 =
        case BlockHeight of
            X when X == Height + 1 ->
                %% as expected, just continue below
                Followers0;
        X when X =< Height ->
                lager:info("ignoring block ~p", [BlockHeight]),
                %% already have these
                acknowledge_blockchain_event(From),
                %% throws count as early returns from gen_servers
                throw({noreply, State});
        X when X > Height + 1 ->
                MapFollowers =
                    fun(MissingHeight, FS) ->
                            {ok, MissingBlock} = blockchain:get_block(MissingHeight, Chain),
                            MissingHash = blockchain_block:hash_block(MissingBlock),
                            {ok, MissingLedger} = case LedgerRequired of
                                                      true ->
                                                          blockchain:ledger_at(MissingHeight, Chain);
                                                      false ->
                                                          {ok, undefined}
                                                  end,
                            lists:map(fun({M, S}) ->
                                              {ok, NS} = M:load(MissingHash, MissingBlock, true, MissingLedger, S),
                                              {M, NS}
                                      end, FS)
                    end,
                %% missing some blocks, try to obtain them
                BlockHeights = lists:seq(Height + 1, BlockHeight - 1),
                lager:info("trying to absorb missing blocks [~p..~p]", [hd(BlockHeights), lists:last(BlockHeights)]),
                lists:foldl(MapFollowers, Followers0, BlockHeights)
        end,

    Followers2 = lists:map(fun({M, S}) ->
                                   {ok, NS} = M:load(Hash, Block, Sync, Ledger, S),
                                   {M, NS}
                           end, Followers1),

    acknowledge_blockchain_event(From),
    {noreply, State#state{followers=Followers2}};

handle_info({blockchain_event, From, Other}, State=#state{}) ->
    lager:info("Ignoring blockchain event: ~p", [Other]),
    acknowledge_blockchain_event(From),
    {noreply, State};

handle_info(_Info, State) ->
    lager:warning("unexpected message ~p", [_Info]),
    {noreply, State}.

terminate(Reason, State=#state{}) ->
    lists:foreach(fun({M, S}) ->
                          case erlang:function_exported(M, terminate, 2) of
                              true->
                                  M:terminate(Reason, S);
                              false ->
                                  ok
                          end
                   end, State#state.followers).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%
%% Internal
%%

register_blockchain_event_handler(Pid) ->
    case application:get_env(blockchain_follower, sync_mode, false) of
        true ->
            blockchain_event:add_sync_handler(Pid);
        false ->
            blockchain_event:add_handler(Pid)
    end.

acknowledge_blockchain_event(From) ->
    case application:get_env(blockchain_follower, sync_mode, false) of
        true ->
            blockchain_event:acknowledge(From);
        false ->
            ok
    end.
