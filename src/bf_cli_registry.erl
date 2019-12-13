%%%-------------------------------------------------------------------
%% @doc bf_cli_registry
%% @end
%%%-------------------------------------------------------------------
-module(bf_cli_registry).

-define(CLI_MODULES, [
                      bf_cli_info,
                      bf_cli_genesis
                     ]).

-export([register_cli/0]).

register_cli() ->
    clique:register(?CLI_MODULES).
