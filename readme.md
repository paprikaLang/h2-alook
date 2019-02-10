#### **一. HTTP/2 features**

- One TCP Connection
- Binary Protocol
- Multiplexed(多路复用)
- Flow Control and Prioritization (WINDOWS_UPDATE,PRIORITY)
- Header Compression
- Server Push
- Encrypted

<img src="/images/h2_upgrade.jpg" width="450px;" />

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
服务端返回的讯息通过 `io:format("Socket got ~w~n",[Data])`显示如下内容:
```
Socket got <<0,0,24,4,0,0,0,0,0,0,5,0,16,0,0,0,3,0,0,0,250,0,6,0,16,1,64,0,4,0,16,0,0>>
```
这里 0,0,24 表示 Length ; 4,0 表示 Type 为 SETTINGS
<div style="display: flex; flex-direction: row; justify-content: space-around;">
<img src="/images/h2_frame.jpg" width="400px;" />
<img src="/images/h2_frames.jpg" width="400px;" /> 
</div>

<img src="/images/h2_type.jpg" width="320px;" />

<div style="display: flex; flex-direction: row; justify-content: space-around;">
    <img src="/images/h2_setting.jpg" width="400px;" />
    <img src="/images/h2_total.jpg" width="400px;" />
</div>










