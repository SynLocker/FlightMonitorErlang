-module(clock).
-export([start/1]).

start(Callback) ->
    timer:sleep(50),
    Planes0 = sky_monitor:get_all_planes(),
    %io:format("~p~n", [Planes0]),
    Planes1 = maps:values(maps:map(fun(_Id, {_,PlaneInfo}) -> PlaneInfo end, Planes0)),
    Planes2 = jsone:encode(Planes1),
    Callback(Planes2),
    start(Callback).