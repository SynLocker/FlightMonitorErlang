%%%-------------------------------------------------------------------
%% @doc sandbox public API
%% @end
%%%-------------------------------------------------------------------

-module(sandbox_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    sky_monitor_engine_start(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/sample", sample_handler, <<"Initial state initialized here">>},
            {"/add/:num1/:num2", [{num1, int}, {num2, int}], add_handler, []},
            {"/post", post_handler, []},
            {"/ws", ws_handler, #{}}
            ]}
    ]),
    %Start a tcp http server on port 8080, with the specified path routing rules on Dispatch
    %cowboy:start_clear(Name,TransportOptions,ProtocolOptions)
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    sandbox_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
sky_monitor_engine_start() ->
    sky_monitor:start_link(),
    plane_dns:start(),
    client_msg_manager:start(),
    plane:test().