-module(quotetest).
-export([generate_get_params/1]).

%% Taken from VoDKATV example used in the PROWESS mid-term workshop
generate_get_params([{_Name, ""}]) ->
    "";
generate_get_params(_) ->
    unknown.

