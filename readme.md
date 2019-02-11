### **HTTP/2 features**

- One TCP Connection
- Binary Protocol
- Multiplexing(多路复用)
- Flow Control and Prioritization (WINDOWS_UPDATE,PRIORITY)
- Header Compression
- Server Push
- Encrypted

<img src="/images/h2_upgrade.jpg" width="450px;" />

 HTTP1.1 响应的文本内容太大，一般使用 chunk 模式进行分块传输. 

 HTTP2.0 可以 Framing 分帧传送, 同一个响应不同的帧共用一个 stream_id ，服务端将具有相同 stream_id 的消息帧串起来作为一个整体来处理，一个流的最后一个消息帧会有一个流结束标志位(EOS——End of Stream). 
 
 EOS 并不意味着连接的中止，同一个连接上还会有其他流穿插传输，即多路复用.

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
~ erl -s ssl
> c(h2),f(P),P=h2:start("http2.golang.org").
```

这里服务端返回的 Frame: 0,0,24 表示 Length ; 4 表示 Type 为 SETTINGS (参照下面几张表).
```
Socket got <<0,0,24,4,0,0,0,0,0,0,5,0,16,0,0,0,3,0,0,0,250,0,6,0,16,1,64,0,4,0,16,0,0>>
```

<div style="display: flex; flex-direction: row; justify-content: space-between;">
<img src="/images/h2_frame.jpg" width="400px;" />
<img src="/images/h2_frames.jpg" width="400px;" /> 
</div>

<img src="/images/h2_type.jpg" width="450px;" />

- 0 - DATA:           对应 HTTP Response Body
- 1 - HEADERS:        对应 HTTP Header
- 3 - RST_STREAM:     终止当前的消息并重新发送一个新的, 避免中断已有的连接
- 5 - PUSH_PROMISE:   服务器向客户端主动推送
- 7 - GOAWAY:         通知对方断开连接
- 8 - WINDOW_UPDATE:  flow control, 调整帧窗口大小, 控制数据帧的流量
- 9 - CONTINUATION:   HEADERS 太大时分块的续帧
- 4 - SETTINGS:       两端交流连接配置的信息包括以下字段

如果有没返回的字段如: HEADER_TABLE_SIZE 和 ENABLE_PUSH , 则采用默认值

<div style="display: flex; flex-direction: row; justify-content: space-between;">
    <img src="/images/h2_setting.jpg" width="410px;" />
    <img src="/images/h2_total.jpg" width="410px;" />
</div>

客户端需要发送一个 Magic 8字节流,再接一个 SETTINGS 帧, 来建立HTTP/2的连接; 只有支持 HTTP/2 的服务器可以识别这个 SETTINGS 帧，并回复一个自己的 SETTINGS 帧表示连接成功。
```
init(Host) ->
    {ok,Socket} = ssl:connect(Host,443,[
        binary,
        {alpn_advertised_protocols,[<<"h2">>]}
        ]),
    Magic = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n",
    Settings = <<0,0,0,4,0,0,0,0,0>>,
    ssl:send(Socket,[Magic,Settings]),
    loop(Socket).
```
服务端返回的信息: 
- 0,0,4,(8) 的 Type 为 WINDOW_UPDATE; 
- <<0,0,0,4,1,0,0,0,0>> Flags 为 1, 是对客户端 Settings <<0,0,0,4,0,0,0,0,0>> 的一个 ack .
```
Socket got <<0,0,24,4,0,0,0,0,0,0,5,0,16,0,0,0,3,0,0,0,250,0,6,0,16,1,64,0,4,0,16,0,0>>
Socket got <<0,0,4,8,0,0,0,0,0,0,15,0,1>>
Socket got <<0,0,0,4,1,0,0,0,0>>
```
解析上面的信息:
```
parse(<<_:24,4,0,_/binary>>) -> io:format("Got SETTINGS~n");
parse(<<_:24,4,1,_/binary>>) -> io:format("Got SETTINGS_ACK~n");
parse(<<_:24,8,_/binary>>) -> io:format("Got WINDOWN_UPDATE~n");
```
```
Got SETTINGS
Got WINDOWN_UPDATE
Got SETTINGS_ACK
```

**Header Compression**

GET 请求需在 Request Headers 中设置 `:method: GET` 等头信息.

而 HTTP/2 连接会对头部信息进行压缩. HPACK 是一种表查找压缩方案，使用了 Huffman 编码. 客户端和服务端各维护了两张这样的表: 一张是静态表，用于查找最常用的 61 个 Header ; 一张动态表，记录 Value 不固定的或者 Name 不在静态表中的 Header, Index 从 62 开始.

<div style="display: flex; flex-direction: row; justify-content: space-between;">
<img src="images/h2_request.jpg" width="300px;">
<img src="images/h2-static.jpg" width="360px;">
</div>

<br>

**HPACK:**

<div style="display: flex; flex-direction: row; justify-content: space-between;">
<img src="images/h2_int.jpg" width="220px;">
<img src="images/h2_string.jpg" width="220px;">
</div>

```
0b10_0001100_0010000 + 31 = 34251
```

静态表中的 Header 压缩后都对应一个固定的值(因为 Name 和 Value 都固定):

<img src="images/h2_index.jpg" width="400px;">

动态表添加 Header:

1. `:authority` 是一个 `indexed name` . 65 代表它在静态表中的 Index=1 ; 16 则是 value `http2.golang.org` 的长度;

2. Header Name 不在静态表中的, 要按照 `new name` 的格式添加到动态表.

3. 成功添加到动态表之后, 第二次再发送这个 Header 就可以在**动态表**中查找对应的 Index 了, 如 190 在动态表中对应的 Index 为 62.

4. 动态表中的 Header 不会无限添加下去, 视 SETTINGS 的 HEADER_TABLE_SIZE 而定.

<div style="display: flex; flex-direction: row; justify-content: space-between;">
<img src="images/h2_http2.jpg" width="350px;">
<img src="images/h2_cache.jpg" width="480px;">
</div>

整合所有头信息:
<div style="display: flex; flex-direction: row; justify-content: space-between;">
<img src="images/h2_final.jpg" width="420px;">
<img src="images/h2_finals.jpg" width="420px;">
</div>

```
 {send,Data} ->
    ssl:send(Socket,Data),
    loop(Socket)
```
进程接收到 send 函数直接通过 `ssl:send` 发送 Data .
```
> P ! {send,<<0,0,21,1,5,0,0,0,1,130,135,132,1,16,104,116,116,112,50,46,103,111,108,97,110,103,46,111,114,103>>}.
```
```
 ... ...
<p>Congratulations, <b>you're using HTTP/2 right now</b>.</p>
 ... ...
```
如果此时再发送一次相同的请求
```
> P ! {send,<<0,0,21,1,5,0,0,0,1,130,135,132,1,16,104,116,116,112,50,46,103,111,108,97,110,103,46,111,114,103>>}.
```
返回的却是: Type = 7 表示 GOAWAY.
```
Got other frame <<0,0,8,7,0,0,0,0,0,0,0,0,1,0,0,0,1>>
```
重新编译, 换不同的 stream_id 尝试又可以正常返回数据了.
```
> c(h2),f(P),P=h2:start("http2.golang.org").
> P ! {send,<<0,0,21,1,5,0,0,0,1,130,135,132,1,16,104,116,116,112,50,46,103,111,108,97,110,103,46,111,114,103>>}.
> P ! {send,<<0,0,21,1,5,0,0,0,3,130,135,132,1,16,104,116,116,112,50,46,103,111,108,97,110,103,46,111,114,103>>}.
```


<br>

**HTTP/2 与 gRPC**

Protobuf 将消息序列化后，内容放在 数据帧的 Payload 字段里，消息的长度在数据帧的 Length 字段里，消息的类名称放在 HEADERS 帧的 Path 头信息中. 我们需要通过头部字段的 Path 来确定具体消息的解码器来反序列化数据帧的 Payload 内容，而这些工作 gRPC 都帮我们干了.
