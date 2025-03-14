-module(fileclient).
-export([main/0, main/1]).

main() ->
  io:format("main.............~w~n", [self()]),
  {ok, Sock} = gen_tcp:connect("your-server-ip", 7788, [binary]),
  receive
    {recv, Path} ->
      file:delete(Path),
      dorecv(Sock, Path)
  end.

main([SendPath]) ->
  io:format("main.............~w ~w~n", [self(), SendPath]),
  {ok, Sock} = gen_tcp:connect("your-server-ip", 7788, [binary]),
  {ok, ByteSent} = file:sendfile(SendPath, Sock),
  io:format("bytesent is ~w~n", [ByteSent]).

dorecv(Sock, Path) ->
  receive
    {tcp, _S, Data} ->
      io:format("recv data.......~w~n", [byte_size(Data)]),
      file:write_file(Path, Data, [append]),
      dorecv(Sock, Path)
  end.