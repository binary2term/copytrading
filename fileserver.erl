-module(fileserver).
-export([main/0]).

main() ->
  io:format("begin of main......~n"),
  {ok, Lsock} = gen_tcp:listen(7788, [{packet, 0}, {reuseaddr, true}]),
  io:format("after listen.......~n"),
  {ok, Sock} = gen_tcp:accept(Lsock),
  io:format("do accept new conn...~w~n", [self()]),
  receive
    {send, Path} ->
      {ok, ByteSent} = file:sendfile(Path, Sock),
      io:format("bytesent is ~w~n", [ByteSent]);
    {recv, Path} ->
      dorecv(Sock, Path)
  end.

dorecv(Sock, Path) ->
  receive
    {tcp, C, Data} ->
      io:format("recvdata: ..........~w ~w ~w ~n", [byte_size(Data), Sock, C]),
      file:write_file(Path, Data, [append]),
      dorecv(Sock, Path)
  end.

