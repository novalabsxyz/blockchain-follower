%%%-------------------------------------------------------------------
%% @doc hf_cli_registry
%% @end
%%%-------------------------------------------------------------------
-module(hf_cli_registry).

-define(CLI_MODULES, [
                      hf_cli_genesis
                     ]).

-export([register_cli/0]).

register_cli() ->
    clique:register(?CLI_MODULES).
