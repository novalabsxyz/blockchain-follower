-module(bf_follower).

-callback requires_sync() -> boolean().
-callback requires_ledger() -> boolean().
-callback follower_height() -> pos_integer().
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
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
        {
         chain=undefined :: undefined | blockchain:blockchain(),
         follower_mod :: atom(),
         follower_state :: any(),
         requires_sync :: boolean(),
         requires_ledger :: boolean()
        }).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Args) ->
    {FollowerMod, FollowerArgs} = proplists:get_value(follower_module, Args),
    FollowerState = FollowerMod:init(FollowerArgs),
    RequiresSync = FollowerMod:requires_sync(),
    RequiresLedger = FollowerMod:requires_ledger(),
    case RequiresSync of
        true -> blockchain_event:add_sync_handler(self());
        false -> blockchain_event:add_handler(self())
    end,

    BaseDir = application:get_env(blockchain, base_dir, "data"),
    blockchain_worker:load(BaseDir, "update"),

    {ok, #state{follower_mod=FollowerMod, follower_state=FollowerState,
                requires_sync=RequiresSync, requires_ledger=RequiresLedger}}.

handle_call(_Request, _From, State) ->
    lager:warning("unexpected call ~p from ~p", [_Request, _From]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    lager:warning("unexpected cast ~p", [_Msg]),
    {noreply, State}.

handle_info({blockchain_event, From, {new_chain, Chain}}, State=#state{follower_mod=FollowerMod}) ->
    {ok, FollowerState} = FollowerMod:follower_load_chain(Chain, State#state.follower_state),
    acknowledge_blockchain_event(From, State),
    {noreply, State#state{chain=Chain, follower_state=FollowerState}};

handle_info({blockchain_event, From, {add_block, Hash, Sync, Ledger}},
            State=#state{chain=Chain, follower_mod=FollowerMod, requires_ledger=RequiresLedger}) ->

    {ok, Block} = blockchain:get_block(Hash, Chain),
    Height = FollowerMod:follower_height(),
    BlockHeight = blockchain_block:height(Block),

    FollowerState1 =
        case BlockHeight of
            X when X == Height + 1 ->
                %% as expected, just continue below
                State#state.follower_state;
        X when X =< Height ->
                lager:info("ignoring block ~p", [BlockHeight]),
                %% already have these
                acknowledge_blockchain_event(From, State),
                %% throws count as early returns from gen_servers
                throw({noreply, State});
        X when X > Height + 1 ->
                %% missing some blocks, try to obtain them
                BlockHeights = lists:seq(Height + 1, BlockHeight - 1),
                lager:info("trying to absorb missing blocks [~p..~p]", [hd(BlockHeights), lists:last(BlockHeights)]),
                lists:foldl(fun(MissingHeight, FS) ->
                                    {ok, MissingBlock} = blockchain:get_block(MissingHeight, Chain),
                                    MissingHash = blockchain_block:hash_block(MissingBlock),
                                    {ok, MissingLedger} = case RequiresLedger of
                                                              true ->
                                                                  blockchain:ledger_at(MissingHeight, Chain);
                                                              false ->
                                                                  {ok, undefined}
                                                          end,
                                    {ok, NewFS} = FollowerMod:load(MissingHash,
                                                                   MissingBlock,
                                                                   true,
                                                                   MissingLedger,
                                                                   FS),
                                    NewFS
                            end, State#state.follower_state, BlockHeights)
        end,

    {ok, FollowersState} =  FollowerMod:load(Hash, Block, Sync, Ledger, FollowerState1),

    acknowledge_blockchain_event(From, State),
    {noreply, State#state{follower_state=FollowersState}};

handle_info({blockchain_event, From, Other}, State=#state{}) ->
    lager:info("Ignoring blockchain event: ~p", [Other]),
    acknowledge_blockchain_event(From, State),
    {noreply, State};

handle_info(_Info, State) ->
    lager:warning("unexpected message ~p", [_Info]),
    {noreply, State}.

terminate(Reason, State=#state{follower_mod=FollowerMod}) ->
    case erlang:function_exported(FollowerMod, terminate, 2) of
        true->
            FollowerMod:terminate(Reason, State#state.follower_state);
        false ->
            ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%
%% Internal
%%

acknowledge_blockchain_event(From, State) ->
    case State#state.requires_sync of
        true ->
            blockchain_event:acknowledge(From);
        false ->
            ok
    end.
