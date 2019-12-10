%%%-------------------------------------------------------------------
%% @doc hf_console
%% @end
%%%-------------------------------------------------------------------
-module(hf_console).

-export([command/1]).

-spec command([string()]) -> ok.
command(Cmd) ->
    clique:run(Cmd).
