Upgrade to Http2

```
-module(h2).
-export([start/1,init/1]).

start(Host) ->
    spawn(h2,init,[Host]).

init(Host) ->
    {ok,Socket} = ssl:connect(Host,443,[
        binary,
        {alpn_advertised_protocols,[<<"h2">>]}
        ]),
    loop(Socket).

loop(Socket) ->
    receive
        {ssl,Socket,Data} ->
            io:format("Socket got ~w~n",[Data]),
            loop(Socket)
    end.
```

```
Socket got <<0,0,24,4,0,0,0,0,0,0,5,0,16,0,0,0,3,0,0,0,250,0,6,0,16,1,64,0,4,0,16,0,0>>
```
