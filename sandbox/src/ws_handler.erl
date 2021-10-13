-module(ws_handler).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-define(TIMEOUT, 120000).

%upgrade to HTTP to WS
init(Req, State) ->
    #{peer := {IP, _Port}} = Req,
    io:format("websocket connection from ~p ~n", [IP]),
    Opts = #{idle_timeout => ?TIMEOUT},
    {cowboy_websocket, Req, State, Opts}.

%% websocket_xx handlers return a {[Frame], State} tuple, that frame is sent back to the client
%% frame types = {text, Data} or {binary, Data}

%post upgrade initialization stuff
websocket_init(State) -> 
    io:format("websocket init callback ~p ~n", [self()]),
    self() ! print_pid,
    client_msg_manager:subscribe(self()), 
    {[{text, "connection successful"}], State}.

websocket_handle({text, <<MsgType:8/binary, MsgContent/binary>>}, State) when MsgType == <<"planemsg">> ->
    io:format("frame received ~p ~n", [MsgContent]),
    spawn(fun() -> plane_msg_executor:exec(MsgContent) end),
    {[{text, MsgContent}], State};

websocket_handle({text, Text}, State) ->
    io:format("frame received ~p ~n", [Text]),
    {[{text, "Unknown command"}], State}.

%called using ! operator
websocket_info(print_pid, State) ->
    io:format("pid of this ws port is: ~p ~n", [self()]),
    {ok, State}; %% this tuple means that the websocket is not going to send any data to the client

websocket_info(stop, State) ->
    io:format("terminating: ~p ~n", [self()]),
    {stop, State};

websocket_info(Info, State) ->
    {[{text, Info}], State}.

terminate(Reason, _Req, _State) ->
    io:format("Terminate reason: ~p ~n", [Reason]),
    client_msg_manager:unsubscribe(self()), 
    ok.
