-module(plane_msg_executor).
-export([exec/1]).

% Msg1 = <<"{\"type\": \"update\", \"plane_id\": \"flight:0\", \"param\": \"angle\", \"value\": 100}">>

exec(Message) ->
    type, create, update, destroy, plane_id, x, y, angle, speed, param, value,  %creating atoms before parsing
    {ok, ParsedMessage, _} = jsone:try_decode(Message, [{keys, attempt_atom}]),
    Type = maps:get(type, ParsedMessage),

    io:format(Type),
    case Type of
        <<"create">> -> 
            handle_create(ParsedMessage);
        <<"update">> -> 
            handle_update(ParsedMessage);
        <<"destroy">> -> 
            handle_destroy(ParsedMessage)
    end.

handle_create(CreateMessage) ->
    PlaneId = maps:get(plane_id, CreateMessage),
    X = maps:get(x, CreateMessage),
    Y = maps:get(y, CreateMessage),
    Angle = maps:get(angle, CreateMessage),
    Speed = maps:get(speed, CreateMessage),
    plane:spawn_plane(PlaneId, X, Y, Angle, Speed).

handle_update(UpdateMessage) ->
    PlaneId = maps:get(plane_id, UpdateMessage),
    PlanePid = plane_dns:retrieve_from_id(PlaneId),
    Param = maps:get(param, UpdateMessage),
    Value = maps:get(value, UpdateMessage),
    case Param of
        <<"angle">> -> plane:update_flight_parameters(PlanePid, {new_angle, Value});
        <<"speed">> -> plane:update_flight_parameters(PlanePid, {new_speed, Value})
    end.

handle_destroy(DestroyMessage) ->
    PlaneId = maps:get(plane_id, DestroyMessage),
    PlanePid = plane_dns:retrieve_from_id(PlaneId),
    plane:destroy_plane(PlanePid).