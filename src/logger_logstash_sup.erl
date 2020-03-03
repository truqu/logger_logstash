-module(logger_logstash_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%==============================================================================================
%% API
%%==============================================================================================

-spec start_link() -> tql:either(pid(), term()).
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%==============================================================================================
%% Supervisor callbacks
%%==============================================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  SupFlags = #{ strategy  => one_for_one
              , intensity => 1
              , period    => 5
              },
  ChildSpec = [#{ id       => logger_logstash_udp
                , type     => worker
                , start    => {logger_logstash_udp, start_link, []}
                , restart  => permanent
                , shutdown => 5000
                , modules  => [logger_logstash_udp]
                }],
  {ok, {SupFlags, ChildSpec}}.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 96
%% coding: utf-8
%% End:
