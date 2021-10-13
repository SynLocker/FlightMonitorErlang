-module(sky_monitor).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, update_plane/1, get_all_planes/0, register_plane/1, destroy_plane/1]).
-behaviour(gen_server).

%The state of this server is defined by a map of planes, which every key is the id of a plane
%and every value is defined as : {LastMessageProg, PlaneInfo}
%PlaneInfo = [{plane_id, PlaneId}, {plane_id, PlaneId}, {x, X},{y, Y},{angle, Angle}, {speed, Speed}]
%messages coming from a plane with MsgProgressive < LastMessageProg must be ignored

%Sky monitor API definition
start_link() ->
    {ok, _} = gen_server:start_link({local, sky_monitor}, ?MODULE, #{}, []).

update_plane({MsgProg, PlaneInfo}) ->
    gen_server:cast(sky_monitor, {MsgProg, PlaneInfo}).

get_all_planes() ->
    gen_server:call(sky_monitor, get_all).

register_plane([{plane_id, PlaneId} | Param]) ->
    gen_server:call(sky_monitor, {register_plane, [{plane_id, PlaneId} | Param]}).

destroy_plane(PlaneId) ->
    gen_server:cast(sky_monitor, {destroy_plane, PlaneId}).

%Gen server callbacks
init(Planes) ->
    {ok, Planes}.

handle_call({register_plane, [{plane_id, PlaneId} | Param]}, _From, Planes0) ->
    Planes1 = maps:put(PlaneId, {0, [{plane_id, PlaneId} | Param]}, Planes0),
    {reply, register_successful, Planes1};

handle_call(get_all, _From, Planes) ->
    {reply, Planes, Planes}.

handle_cast({destroy_plane, PlaneId}, Planes0) ->
    Planes1 = maps:remove(PlaneId, Planes0),
    {noreply, Planes1};

handle_cast({MsgProg, [{plane_id, PlaneId} | Param]}, Planes0) -> 
    {LastMessageProg, _CurrentPlaneInfo} = maps:get(PlaneId, Planes0),
    if 
        LastMessageProg < MsgProg ->
            Planes1 = maps:update(PlaneId, {MsgProg, [{plane_id, PlaneId} | Param]}, Planes0), 
            {noreply, Planes1};
        LastMessageProg >= MsgProg -> 
            {noreply, Planes0}
    end.

handle_info(_Msg, State) ->
    %io:format("SkyMonitor received ~p ~n", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, _State) ->
    io:format("SkyMonitor terminated with reason: ~p~n", [Reason]),
    ok.