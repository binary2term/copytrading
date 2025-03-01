-module(ea1_api).
-behaviour(gen_server).

-export([start_link/0, handle_call/3, init/1, handle_info/2, sign_hmac/1,
         send_http/3, send_http/1]).

-define(SECRET_KEY, "your-binance-secretkey").
-define(API_KEY, "your-binance-apikey").

start_link() ->
  gen_server:start_link({local, ea1_api}, ea1_api, [], []).

% ea1_api:send_http(post, "/fapi/v1/order", "symbol=BTCUSDT&side=BUY&type=MARKET&positionSide=LONG&quantity=0.002").

% ea1_api:send_http(get, "/fapi/v1/klines", "symbol=BTCUSDT&interval=15m&limit=200").

% list_to_integer(lists:concat(tuple_to_list(erlang:timestamp()))).
% erlang:system_time() div 10.

init([]) ->
  io:format("init in api self: ~w~n", [self()]),
  erlang:send_after(1000, ea1_api, reqhttp),
  {ok, []}.

sign_hmac(Message) ->
  R1 = crypto:mac(hmac, sha256, ?SECRET_KEY, Message),
  R2 = binary_to_list(R1),
  Func = fun(E, A) ->
    if
      E < 16 -> ["0" | [string:to_lower(integer_to_list(E, 16)) | A]];
      true -> [string:to_lower(integer_to_list(E, 16)) | A]
    end
  end,
  lists:flatten(lists:foldr(Func, [], R2)).

send_http(Path) ->
  Url = lists:concat(["https://fapi.binance.com", Path]),
  httpc:request(get, {Url, [{"X-MBX-APIKEY", ?API_KEY}]}, [], []).

send_http(get, Path, Qs) ->
  T1 = erlang:system_time() div 1000000,
  T2 = integer_to_list(T1),
  Qs1 = case Qs of "" -> lists:concat(["timestamp=", T2]); _ -> lists:concat([Qs, "&timestamp=", T2]) end,
  Sign = sign_hmac(Qs1),
  Url = lists:concat(["https://fapi.binance.com", Path, "?", Qs1, "&signature=", Sign]),
  io:format("full url is: ~s~n", [Url]),
  httpc:request(get, {Url, [{"X-MBX-APIKEY", ?API_KEY}]}, [], []);

send_http(post, Path, Qs) ->
  T1 = erlang:system_time() div 1000000,
  T2 = integer_to_list(T1),
  Qs1 = case Qs of "" -> lists:concat(["timestamp=", T2]); _ -> lists:concat([Qs, "&timestamp=", T2]) end,
  Sign = sign_hmac(Qs1),
  Url = lists:concat(["https://fapi.binance.com", Path, "?", Qs1, "&signature=", Sign]),
  io:format("full url is: ~s~n", [Url]),
  httpc:request(post, {Url, [{"X-MBX-APIKEY", ?API_KEY}], ["application/x-www-form-urlencoded"], []}, [], []).

handle_call(Req, From, State) ->
  io:format("handle call in api self: ~w~n", [self()]),
  case Req of
    {chart, Symbol, Period} ->
      % io:format("handle_call chart symbol: ~p, period:~p~n", [Symbol, Period]),
      Httpret = send_http(get, "/fapi/v1/klines", lists:concat(["symbol=", binary_to_list(Symbol), "&interval=", binary_to_list(Period), "&limit=200"])),
      % io:format("httpret: ~p~n", [Httpret]),
      Data = element(3, element(2, Httpret)),
      {reply, list_to_binary(Data), State};
    {order1, Symbol, Side, Sl, Tp, Quantity} ->
      % io:format("handle_call order1 symbol: ~p, side: ~p, Sl: ~p, Tp: ~p, quantity: ~p~n", [Symbol, Side, Sl, Tp, Quantity]),
      case Side of
        <<"SELL">> ->
          {ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}} = send_http(post, "/fapi/v1/order", lists:concat(["symbol=", binary_to_list(Symbol), "&side=SELL&type=MARKET&positionSide=SHORT&quantity=", binary_to_list(Quantity)])),
          {ok, {{_, 200, _}, _, _}} = send_http(post, "/fapi/v1/order", lists:concat(["symbol=", binary_to_list(Symbol), "&side=BUY&type=STOP_MARKET&positionSide=SHORT&stopPrice=", binary_to_list(Sl), "&quantity=", binary_to_list(Quantity)])),
          {ok, {{_, 200, _}, _, _}} = send_http(post, "/fapi/v1/order", lists:concat(["symbol=", binary_to_list(Symbol), "&side=BUY&type=TAKE_PROFIT_MARKET&positionSide=SHORT&stopPrice=", binary_to_list(Tp), "&quantity=", binary_to_list(Quantity)]));
        <<"BUY">> ->
          {ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}} = send_http(post, "/fapi/v1/order", lists:concat(["symbol=", binary_to_list(Symbol), "&side=BUY&type=MARKET&positionSide=LONG&quantity=", binary_to_list(Quantity)])),
          {ok, {{_, 200, _}, _, _}} = send_http(post, "/fapi/v1/order", lists:concat(["symbol=", binary_to_list(Symbol), "&side=SELL&type=STOP_MARKET&positionSide=LONG&stopPrice=", binary_to_list(Sl), "&quantity=", binary_to_list(Quantity)])),
          {ok, {{_, 200, _}, _, _}} = send_http(post, "/fapi/v1/order", lists:concat(["symbol=", binary_to_list(Symbol), "&side=SELL&type=TAKE_PROFIT_MARKET&positionSide=LONG&stopPrice=", binary_to_list(Tp), "&quantity=", binary_to_list(Quantity)]))
      end,
      % io:format("httpret: ~p~n", [Httpret]),
      % handle_call order1 symbol: <<"DOGEUSDT">>, side: <<"SELL">>, Sl: <<"0.35350">>, Tp: <<"0.34962">>, quantity: <<"3097">>
      {reply, <<"ok fine">>, State}
  end.

handle_cast(Req, State) ->
  {noreply, []}.

handle_info(reqhttp, State) ->
  io:format("receive reqhttp cmd.............~w~n", [self()]),
  {noreply, State}.