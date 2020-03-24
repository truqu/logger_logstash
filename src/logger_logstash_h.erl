-module(logger_logstash_h).

%% Handler callbacks
-export([ adding_handler/1
        , log/2
        , change_config/3
        ]).

-type config() :: #{ port   := inet:port_number()
                   , host   := inet:hostname()
                   , fields := [{atom(), jsx:json_term()}]
                   }.

-type init_error() :: undefined_host
                    | undefined_port
                    | {bad_fields, term()}
                    | {invalid_port, atom()}
                    | {failed_to_connect, inet:posix()}.

-define(FORMATTER, {logger_formatter, #{}}).

%%==============================================================================================
%% Handler callbacks
%%==============================================================================================

-spec adding_handler(Config) -> {ok, Config} | {error, term()} when
    Config :: logger:handler_config().
adding_handler(HandlerConfig = #{config := UserConfig}) ->
  case validate_user_config(UserConfig) of
    {ok, #{host := Host} = Config} ->
      {ok, _} = inet:gethostbyname(Host),
      {ok, HandlerConfig#{config => Config}};
    {error, _} = Res -> Res
  end.

-spec log(logger:log_event(), logger:handler_config()) -> ok.
log(#{level := info, meta := #{error_logger := #{type := progress}}}, _) -> ok;
log( #{level := Level, msg := EventData, meta := Meta}
   , #{config := #{host := Host, port := Port, fields := DefaultFields}}
   ) ->
  {Msg, MsgFields} = format_msg(EventData),
  Fields = [{severity, Level}] ++ safe_meta(Meta) ++ MsgFields ++ DefaultFields,
  Data =
    #{ fields => Fields
     , '@timestamp' => format_timestamp(Meta)
     , message => Msg
     },
  logger_logstash_udp:send(Host, Port, jsx:encode(Data)).

-spec change_config(set | update, OldConfig, NewConfig) -> {ok, Config} | {error, term()} when
    OldConfig :: Config,
    NewConfig :: Config,
    Config :: logger:handler_config().
change_config(set, _, #{config := UserConfig} = Config) ->
  case validate_user_config(UserConfig) of
    {ok, #{host := Host} = NewConfig} ->
      {ok, _} = inet:gethostbyname(Host),
      {ok, Config#{config => NewConfig}};
    {error, E} -> {error, E}
  end;
change_config(update, #{config := OldConfig}, #{config := UserConfig0} = Config) ->
  UserConfig = maps:merge(OldConfig, UserConfig0),
  case validate_user_config(UserConfig) of
    {ok, #{host := Host} = NewConfig} ->
      {ok, _} = inet:gethostbyname(Host),
      {ok, Config#{config => NewConfig}};
    {error, E} -> {error, E}
  end.

%%==============================================================================================
%% Internal functions
%%==============================================================================================

-spec format_msg(Data) -> {binary(), [{binary() | atom(), jsx:json_term()}]} when
    Data ::  {io:format(), [term()]}
           | {report, logger:report()}
           | {string, unicode:chardata()}.
format_msg({string, Message}) -> {unicode:characters_to_binary(Message), []};
format_msg({report, Report}) when is_map(Report) -> format_msg({report, maps:to_list(Report)});
format_msg({report, Report}) when is_list(Report) ->
  {proplists:get_value(msg, Report, null), safe_fields(Report)};
format_msg({Format, Params}) ->
  {unicode:characters_to_binary(io_lib:format(Format, Params)), []}.

-spec format_timestamp(logger:metadata()) -> binary().
format_timestamp(#{time := Ts}) ->
  list_to_binary(calendar:system_time_to_rfc3339(Ts, [{unit, microsecond}, {offset, "Z"}])).

-spec safe_meta(logger:metadata()) -> [{binary() | atom(), jsx:json_term()}].
safe_meta(Meta) -> safe_fields(maps:to_list(Meta)).

-spec safe_fields([{term(), term()}]) -> [{binary() | atom(), jsx:json_term()}].
safe_fields(Terms) -> lists:map(fun safe_field/1, Terms).

-spec safe_field({term(), term()}) -> {atom() | binary(), jsx:json_term()}.
safe_field({Key, Value}) when is_atom(Key); is_binary(Key) -> {Key, safe_value(Value)};
safe_field({Key, Value}) when is_list(Key) -> safe_field({list_to_binary(Key), Value}).

-spec safe_value(term()) -> jsx:json_term().
safe_value(Pid) when is_pid(Pid) -> list_to_binary(pid_to_list(Pid));
safe_value(List) when is_list(List) ->
  case io_lib:char_list(List) of
    true -> list_to_binary(List);
    false -> lists:map(fun safe_value/1, List)
  end;
safe_value(Val) when is_binary(Val);
                     is_atom(Val);
                     is_integer(Val) ->
  Val;
safe_value(Val) -> unicode:characters_to_binary(io_lib:format("~p", [Val])).

-spec validate_user_config(map()) -> {ok, config()} | {error, init_error()}.
validate_user_config(Data) when is_map(Data) ->
  Host = maps:get(host, Data, undefined),
  Port = maps:get(port, Data, undefined),
  Fields = maps:get(fields, Data, []),
  validate_opts([ {port, Port}
                , {host, Host}
                , {fields, Fields}
                ], #{});
validate_user_config(_) -> {error, invalid_config}.

-spec validate_opts([{atom(), term()}], map()) -> {ok, config()} | {error, init_error()}.
validate_opts([{Key, Value} | Rest], Acc) ->
  case validate_option(Key, Value) of
    ok -> validate_opts(Rest, Acc#{Key => Value});
    {error, Error} -> {error, Error}
  end;
validate_opts([], Acc) -> {ok, Acc}.

-spec validate_option(atom(), term()) -> ok | {error, init_error()}.
validate_option(port, Port) when is_integer(Port), Port >= 1, Port =< 65536 -> ok;
validate_option(port, undefined) -> {error, undefined_port};
validate_option(port, Port) -> {error, {invalid_port, Port}};
validate_option(host, undefined) -> {error, undefined_host};
validate_option(host, _) -> ok;
validate_option(fields, Fields) when is_list(Fields) -> ok;
validate_option(fields, Fields) -> {error, {bad_fields, Fields}}.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 96
%% coding: utf-8
%% End:
