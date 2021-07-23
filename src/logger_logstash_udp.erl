-module(logger_logstash_udp).
-behaviour(gen_server).

%% Public API
-export([send/3]).

%% Supervisor API
-export([start_link/0]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , terminate/2
        ]).

-type state() :: gen_udp:socket().

%%==============================================================================================
%% Public API
%%==============================================================================================

-spec send(inet:hostname(), inet:port_number(), binary()) -> ok.
send(Host, Port, Data) ->
  case whereis(?MODULE) of
    undefined -> ok;
    _ -> gen_server:call(?MODULE, {send, Host, Port, Data})
  end.

%%==============================================================================================
%% Supervisor API
%%==============================================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%==============================================================================================
%% Supervisor callbacks
%%==============================================================================================

-spec init(term()) -> {ok, state()} | {stop, term()}.
init(_) ->
  {ok, connect()}.

-spec handle_call(term(), term(), state()) -> {reply, ok, state()}.
handle_call({send, Host, Port, Data}, _, Socket) ->
  Socket0 = try_send(Socket, Host, Port, Data),
  {reply, ok, Socket0}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_, State) -> {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_, Socket) -> gen_udp:close(Socket).

%%==============================================================================================
%% Internal functions
%%==============================================================================================

-spec connect() -> gen_udp:socket().
connect() ->
  {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
  Socket.

-spec try_send(Socket, Host :: term(), Port :: term(), Data :: term()) -> Socket when
    Socket :: gen_udp:socket().
try_send(Socket, Host, Port, Data) ->
  case gen_udp:send(Socket, Host, Port, Data) of
    ok -> Socket;
    {error, closed} -> try_send(connect(), Host, Port, Data);
    {error, _} -> Socket
  end.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 96
%% coding: utf-8
%% End:
