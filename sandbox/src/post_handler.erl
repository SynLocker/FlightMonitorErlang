-module(post_handler).
-export([init/2]).

init(Req0, State) ->
    #{method := Method} = Req0,

    Req = case Method of
            <<"POST">> ->
                {ok, Data, Req1} = cowboy_req:read_body(Req0),
                cowboy_req:reply(200,
                    #{<<"content-type">> => <<"text/plain">>},
                    [<<"POST body is: ">>, Data, <<"\n">>], Req1
                );
            _ ->
                cowboy_req:reply(405,
                    #{<<"content-type">> => <<"text/plain">>},
                    <<"Method not supported! \n">>, Req0
                )
        end,
    {ok, Req, State}.
                    