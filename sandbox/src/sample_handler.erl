-module(sample_handler).
-export([init/2]).

%function required by all http handler
%Req0 contains information about the request, State is passed by the dispatcher
init(Req0, State) ->
    io:format("request incoming~n"),

    %cowboy_req:reply(HttpStatus, HttpHeader, HttpBody, Request)
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        State, Req0
    ),

    {ok, Req, State}.

