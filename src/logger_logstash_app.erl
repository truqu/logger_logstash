-module(logger_logstash_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%==============================================================================================
%% Application callbacks
%%==============================================================================================

-spec start(term(), term()) -> {ok, pid()}.
start(_, _) -> logger_logstash_sup:start_link().

-spec stop(term()) -> ok.
stop(_) -> ok.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 96
%% coding: utf-8
%% End:
