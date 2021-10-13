-module(plane_dns).
-export([start/0, loop/1, retrieve_from_id/1, register_plane/2, unregister_plane/1]).

start() ->
    Pid = spawn(?MODULE, loop, [#{}]),
    register(plane_pid_dns, Pid).

loop(PlanesPidMap) ->
    receive
        {retreive, PlaneId, RequesterPid} ->
            PlanePid = maps:get(PlaneId, PlanesPidMap, no_match),
            RequesterPid ! {plane_pid, PlanePid},
            loop(PlanesPidMap);
        {register, PlaneId, PlanePid} ->
            loop(maps:put(PlaneId, PlanePid, PlanesPidMap));    
        {unregister, PlaneId} ->
            loop(maps:remove(PlaneId, PlanesPidMap))
    end.

retrieve_from_id(PlaneId) ->
    plane_pid_dns ! {retreive, PlaneId, self()},
    receive
        {plane_pid, PlanePid} -> PlanePid
    end.

register_plane(PlaneId, PlanePid) ->
    plane_pid_dns ! {register, PlaneId, PlanePid}.

unregister_plane(PlaneId) ->
    plane_pid_dns ! {unregister, PlaneId}.