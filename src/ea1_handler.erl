-module(ea1_handler).
-export[init/2].

init(Req, State) ->
  {IP,Port} = cowboy_req:peer(Req),
  {{Y,Mon,D},{H,M,S}} = calendar:local_time(),
  io:format("------------------------------------------~w-~w-~w ~w:~w:~w from ~s:~w~n",
  [Y,Mon,D,H,M,S,string:join([integer_to_list(Ele) || Ele <- tuple_to_list(IP)], "."), Port]),
  io:format("init self pid ~w~n", [self()]),
  Path = cowboy_req:path(Req),
  % io:format("Path is ~p~n", [Path]),
  Rsphead = #{
    <<"content-type">> => <<"text/plain">>,
    <<"Access-Control-Allow-Origin">> => <<"*">>
  },
  {ok, KVs, Req1} = cowboy_req:read_urlencoded_body(Req),
  % io:format("kvs is: ~p~n", [KVs]),
  case Path of
    <<"/chart">> ->
      {_, Symbol} = lists:keyfind(<<"symbol">>, 1, KVs),
      {_, Period} = lists:keyfind(<<"period">>, 1, KVs),
      % io:format("symbol: ~p, period: ~p~n", [Symbol, Period]),
      Binret = gen_server:call(ea1_api, {chart, Symbol, Period}),
      Reqret = cowboy_req:reply(200, Rsphead, Binret, Req),
      {ok, Reqret, State};
    <<"/order1">> ->
      {_, Symbol} = lists:keyfind(<<"symbol">>, 1, KVs),
      {_, Side} = lists:keyfind(<<"side">>, 1, KVs),
      {_, Sl} = lists:keyfind(<<"sl">>, 1, KVs),
      {_, Tp} = lists:keyfind(<<"tp">>, 1, KVs),
      {_, Quantity} = lists:keyfind(<<"quantity">>, 1, KVs),
      Binret = gen_server:call(ea1_api, {order1, Symbol, Side, Sl, Tp, Quantity}),
      Reqret = cowboy_req:reply(200, Rsphead, Binret, Req),
      {ok, Reqret, State};
    <<"/order2">> ->
      order2
  end.
