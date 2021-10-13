-module(client_msg_manager).
-export([start/0, loop/2, update_client_loop/1, subscribe/1, unsubscribe/1]).

-define(UPDATE_RATIO, 1000).

start() ->
    ClientUpdaterPid = spawn(?MODULE, update_client_loop, [[]]),
    ClientPidManager = spawn(?MODULE, loop, [[], ClientUpdaterPid]),
    register(plane_info_updater, ClientPidManager).

loop(PidList, UpdaterPid) ->
    receive 
        {subscribe, Pid} -> loop([Pid | PidList], UpdaterPid);
        {unsubscribe, Pid} -> loop(lists:delete(Pid, PidList), UpdaterPid);
        {get_pid_list} -> UpdaterPid ! {clients_pid_list, PidList}, loop(PidList, UpdaterPid)
    end.

update_client_loop([]) ->
    timer:sleep(?UPDATE_RATIO),
    plane_info_updater ! {get_pid_list},
    receive 
        {clients_pid_list, UpdatedPidList} -> update_client_loop(UpdatedPidList)
    end;

update_client_loop(PidList) ->
    timer:sleep(?UPDATE_RATIO),
    Planes0 = sky_monitor:get_all_planes(),
    %io:format("~p~n", [Planes0]),
    Planes1 = maps:values(maps:map(fun(_Id, {_,PlaneInfo}) -> PlaneInfo end, Planes0)),
    Planes2 = [<<"plane_info_update">>, jsone:encode(Planes1)], %iolist, to avoid the concatenation and making this O(1)
    lists:foreach(fun(ClientPid) -> 
        ClientPid ! Planes2
    end, PidList),
    plane_info_updater ! {get_pid_list},
    receive 
        {clients_pid_list, UpdatedPidList} -> update_client_loop(UpdatedPidList)
    end.

subscribe(ClientPid) ->
    plane_info_updater ! {subscribe, ClientPid}.

unsubscribe(ClientPid) ->
    plane_info_updater ! {unsubscribe, ClientPid}.