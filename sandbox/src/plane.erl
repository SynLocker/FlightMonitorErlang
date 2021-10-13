-module(plane).
-export([
    start/1, fly/2,
    update_flight_parameters/2, update_flight_parameters/3,
    test/0, spawn_plane/5, destroy_plane/1]).

-define(UPDATE_RATIO, 500).

start([{plane_id, PlaneId}, {x, X},{y, Y},{angle, Angle}, {speed, Speed}]) ->
    PlanePid = spawn(?MODULE, fly, [[{plane_id, PlaneId},{x, X},{y, Y},{angle, Angle}, {speed, Speed}], 0]),
    plane_dns:register_plane(PlaneId, PlanePid),
    ok.

fly([{plane_id, PlaneId},{x, X},{y, Y},{angle, Angle}, {speed, Speed}], MessageProg) ->
    timer:sleep(?UPDATE_RATIO),
    UpdatedCoords = calculate_new_coords(consume_all_messages([{plane_id, PlaneId},{x, X},{y, Y},{angle, Angle}, {speed, Speed}])),
    sky_monitor:update_plane({MessageProg, UpdatedCoords}),
    fly(UpdatedCoords, MessageProg + 1).

consume_all_messages([{plane_id, PlaneId},{x, X},{y, Y},{angle, Angle}, {speed, Speed}]) ->
    receive
        {new_angle, NewAngle, new_speed, NewSpeed} ->
            consume_all_messages([{plane_id, PlaneId},{x, X},{y, Y},{angle, NewAngle}, {speed, NewSpeed}]);
        {new_angle, NewAngle} ->
            consume_all_messages([{plane_id, PlaneId},{x, X},{y, Y},{angle, NewAngle}, {speed, Speed}]);    
        {new_speed, NewSpeed} ->
            consume_all_messages([{plane_id, PlaneId},{x, X},{y, Y},{angle, Angle}, {speed, NewSpeed}]);
        {destroy} ->
            sky_monitor:destroy_plane(PlaneId),
            plane_dns:unregister_plane(PlaneId),
            exit(destroyed);
        Other ->
            io:format("Unknow message: ~p ~n", [Other]),
            consume_all_messages([{plane_id, PlaneId},{x, X},{y, Y},{angle, Angle}, {speed, Speed}])
    after 0 ->
        [{plane_id, PlaneId},{x, X},{y, Y},{angle, Angle}, {speed, Speed}]
    end.

calculate_new_coords([{plane_id, PlaneId},{x, X},{y, Y},{angle, Angle}, {speed, Speed}]) ->
    NewX = rotate_coords(X + math:cos(Angle * 0.017453292519943295) * (?UPDATE_RATIO / 1000 * Speed)),   % 0.017453292519943295 = pi / 180
    NewY = rotate_coords(Y + math:sin(Angle * 0.017453292519943295) * (?UPDATE_RATIO / 1000 * Speed)),   % 0.017453292519943295 = pi / 180
    [{plane_id, PlaneId},{x, NewX},{y, NewY},{angle, Angle}, {speed, Speed}].


update_flight_parameters(Plane, NewAngle, NewSpeed) ->
    Plane ! {new_angle, NewAngle, new_speed, NewSpeed}.

update_flight_parameters(Plane, {new_angle, NewAngle}) ->
    Plane ! {new_angle, NewAngle};

update_flight_parameters(Plane, {new_speed, NewSpeed}) ->
    Plane ! {new_speed, NewSpeed}.

destroy_plane(Plane) ->
    Plane ! {destroy}.

%test() ->
%   TowerFun = fun(F) -> receive X -> io:format("~p~n", [X]), F(F) end end,
%   register(control_tower_pid, spawn(fun() -> TowerFun(TowerFun) end)),
%   plane:start([{plane_id, "ZioAirlines"},{x, 0}, {y, 0}, {angle, 0}, {speed, 1}]).

test() ->
    Planes = [
        [{plane_id, <<"flight:0">>},{x, -502}, {y, 843}, {angle, 6}, {speed, 0.15}],
        [{plane_id, <<"flight:1">>},{x, -970}, {y, 806}, {angle, 79}, {speed, 0.46}],
        [{plane_id, <<"flight:2">>},{x, 809}, {y, 84}, {angle, 30}, {speed, 0.04}],
        [{plane_id, <<"flight:3">>},{x, -397}, {y, 961}, {angle, 6}, {speed, 0.77}],
        [{plane_id, <<"flight:4">>},{x, -643}, {y, -324}, {angle, 139}, {speed, 0.95}],
        [{plane_id, <<"flight:5">>},{x, 187}, {y, -118}, {angle, 32}, {speed, 0.37}],
        [{plane_id, <<"flight:6">>},{x, 913}, {y, -564}, {angle, 189}, {speed, 0.89}],
        [{plane_id, <<"flight:7">>},{x, -863}, {y, -259}, {angle, 133}, {speed, 0.05}],
        [{plane_id, <<"flight:8">>},{x, -69}, {y, 871}, {angle, 329}, {speed, 0.06}],
        [{plane_id, <<"flight:9">>},{x, 141}, {y, 840}, {angle, 225}, {speed, 0.8}],
        [{plane_id, <<"flight:10">>},{x, -645}, {y, 734}, {angle, 201}, {speed, 0.87}],
        [{plane_id, <<"flight:11">>},{x, 608}, {y, -130}, {angle, 123}, {speed, 0.66}],
        [{plane_id, <<"flight:12">>},{x, -530}, {y, -445}, {angle, 1}, {speed, 0.11}],
        [{plane_id, <<"flight:13">>},{x, 781}, {y, -131}, {angle, 134}, {speed, 0.46}],
        [{plane_id, <<"flight:14">>},{x, -15}, {y, 791}, {angle, 119}, {speed, 0.84}],
        [{plane_id, <<"flight:15">>},{x, -731}, {y, -307}, {angle, 223}, {speed, 0.61}],
        [{plane_id, <<"flight:16">>},{x, -212}, {y, -592}, {angle, 201}, {speed, 0.06}],
        [{plane_id, <<"flight:17">>},{x, -937}, {y, 468}, {angle, 241}, {speed, 0.31}],
        [{plane_id, <<"flight:18">>},{x, 761}, {y, 422}, {angle, 145}, {speed, 0.13}],
        [{plane_id, <<"flight:19">>},{x, -34}, {y, -583}, {angle, 9}, {speed, 0.83}],
        [{plane_id, <<"flight:20">>},{x, -351}, {y, -159}, {angle, 257}, {speed, 0.21}],
        [{plane_id, <<"flight:21">>},{x, -561}, {y, -126}, {angle, 39}, {speed, 0.99}],
        [{plane_id, <<"flight:22">>},{x, 390}, {y, -652}, {angle, 120}, {speed, 0.38}],
        [{plane_id, <<"flight:23">>},{x, 960}, {y, -565}, {angle, 282}, {speed, 0.88}],
        [{plane_id, <<"flight:24">>},{x, -642}, {y, 56}, {angle, 64}, {speed, 0.67}],
        [{plane_id, <<"flight:25">>},{x, -792}, {y, 324}, {angle, 112}, {speed, 0.27}],
        [{plane_id, <<"flight:26">>},{x, -270}, {y, -621}, {angle, 310}, {speed, 0.8}],
        [{plane_id, <<"flight:27">>},{x, -961}, {y, -52}, {angle, 306}, {speed, 0.06}],
        [{plane_id, <<"flight:28">>},{x, 460}, {y, -698}, {angle, 21}, {speed, 0.28}],
        [{plane_id, <<"flight:29">>},{x, 642}, {y, -692}, {angle, 288}, {speed, 0.96}],
        [{plane_id, <<"flight:30">>},{x, -17}, {y, -385}, {angle, 220}, {speed, 0.92}],
        [{plane_id, <<"flight:31">>},{x, 826}, {y, -854}, {angle, 79}, {speed, 0.11}],
        [{plane_id, <<"flight:32">>},{x, 284}, {y, 79}, {angle, 270}, {speed, 0.36}],
        [{plane_id, <<"flight:33">>},{x, -25}, {y, -46}, {angle, 284}, {speed, 0.01}],
        [{plane_id, <<"flight:34">>},{x, -670}, {y, 820}, {angle, 62}, {speed, 0.13}],
        [{plane_id, <<"flight:35">>},{x, -118}, {y, -270}, {angle, 2}, {speed, 0.83}],
        [{plane_id, <<"flight:36">>},{x, 517}, {y, -419}, {angle, 79}, {speed, 0.31}],
        [{plane_id, <<"flight:37">>},{x, 581}, {y, -31}, {angle, 258}, {speed, 0.06}],
        [{plane_id, <<"flight:38">>},{x, -666}, {y, -6}, {angle, 61}, {speed, 0.52}],
        [{plane_id, <<"flight:39">>},{x, 908}, {y, 247}, {angle, 209}, {speed, 0.66}],
        [{plane_id, <<"flight:40">>},{x, -315}, {y, -8}, {angle, 148}, {speed, 0.61}],
        [{plane_id, <<"flight:41">>},{x, -296}, {y, -803}, {angle, 206}, {speed, 0.1}],
        [{plane_id, <<"flight:42">>},{x, -995}, {y, 707}, {angle, 221}, {speed, 0.35}],
        [{plane_id, <<"flight:43">>},{x, 89}, {y, -179}, {angle, 206}, {speed, 0.67}],
        [{plane_id, <<"flight:44">>},{x, -330}, {y, -860}, {angle, 43}, {speed, 0.58}],
        [{plane_id, <<"flight:45">>},{x, -682}, {y, -290}, {angle, 81}, {speed, 0.32}],
        [{plane_id, <<"flight:46">>},{x, 613}, {y, -204}, {angle, 338}, {speed, 0.5}],
        [{plane_id, <<"flight:47">>},{x, 233}, {y, 598}, {angle, 260}, {speed, 0.25}],
        [{plane_id, <<"flight:48">>},{x, -919}, {y, -272}, {angle, 354}, {speed, 0.4}],
        [{plane_id, <<"flight:49">>},{x, -15}, {y, 912}, {angle, 245}, {speed, 0.19}],
        [{plane_id, <<"flight:50">>},{x, -621}, {y, 862}, {angle, 168}, {speed, 0.05}],
        [{plane_id, <<"flight:51">>},{x, 384}, {y, 45}, {angle, 49}, {speed, 0.64}],
        [{plane_id, <<"flight:52">>},{x, -192}, {y, 983}, {angle, 52}, {speed, 0.22}],
        [{plane_id, <<"flight:53">>},{x, -961}, {y, 93}, {angle, 31}, {speed, 0.32}],
        [{plane_id, <<"flight:54">>},{x, 409}, {y, 208}, {angle, 68}, {speed, 0.35}],
        [{plane_id, <<"flight:55">>},{x, -269}, {y, 356}, {angle, 359}, {speed, 0.51}],
        [{plane_id, <<"flight:56">>},{x, 127}, {y, -166}, {angle, 321}, {speed, 0.59}],
        [{plane_id, <<"flight:57">>},{x, 302}, {y, -536}, {angle, 112}, {speed, 0.63}],
        [{plane_id, <<"flight:58">>},{x, -187}, {y, 751}, {angle, 78}, {speed, 0.78}],
        [{plane_id, <<"flight:59">>},{x, -575}, {y, -295}, {angle, 344}, {speed, 0.19}],
        [{plane_id, <<"flight:60">>},{x, 819}, {y, 209}, {angle, 173}, {speed, 0.59}],
        [{plane_id, <<"flight:61">>},{x, 980}, {y, -419}, {angle, 205}, {speed, 0.9}],
        [{plane_id, <<"flight:62">>},{x, 744}, {y, -17}, {angle, 147}, {speed, 0.75}],
        [{plane_id, <<"flight:63">>},{x, 934}, {y, -30}, {angle, 114}, {speed, 0.47}],
        [{plane_id, <<"flight:64">>},{x, 327}, {y, -763}, {angle, 241}, {speed, 0.33}],
        [{plane_id, <<"flight:65">>},{x, 827}, {y, -651}, {angle, 358}, {speed, 0.13}],
        [{plane_id, <<"flight:66">>},{x, -883}, {y, -763}, {angle, 155}, {speed, 0.48}],
        [{plane_id, <<"flight:67">>},{x, -308}, {y, -10}, {angle, 263}, {speed, 0.74}],
        [{plane_id, <<"flight:68">>},{x, -271}, {y, 191}, {angle, 101}, {speed, 0.61}],
        [{plane_id, <<"flight:69">>},{x, -690}, {y, 519}, {angle, 113}, {speed, 0.86}],
        [{plane_id, <<"flight:70">>},{x, -920}, {y, -63}, {angle, 41}, {speed, 0.56}],
        [{plane_id, <<"flight:71">>},{x, -635}, {y, 332}, {angle, 264}, {speed, 0.95}],
        [{plane_id, <<"flight:72">>},{x, -17}, {y, 253}, {angle, 139}, {speed, 0.06}],
        [{plane_id, <<"flight:73">>},{x, 774}, {y, -887}, {angle, 136}, {speed, 0.87}],
        [{plane_id, <<"flight:74">>},{x, -195}, {y, -276}, {angle, 313}, {speed, 0.44}],
        [{plane_id, <<"flight:75">>},{x, -336}, {y, -777}, {angle, 122}, {speed, 0.24}],
        [{plane_id, <<"flight:76">>},{x, 656}, {y, 851}, {angle, 57}, {speed, 0.5}],
        [{plane_id, <<"flight:77">>},{x, -762}, {y, -16}, {angle, 136}, {speed, 0.69}],
        [{plane_id, <<"flight:78">>},{x, 144}, {y, 464}, {angle, 158}, {speed, 0.95}],
        [{plane_id, <<"flight:79">>},{x, 595}, {y, -985}, {angle, 243}, {speed, 0.3}],
        [{plane_id, <<"flight:80">>},{x, 191}, {y, -170}, {angle, 239}, {speed, 0.39}],
        [{plane_id, <<"flight:81">>},{x, 7}, {y, 326}, {angle, 355}, {speed, 0.5}],
        [{plane_id, <<"flight:82">>},{x, 668}, {y, -575}, {angle, 154}, {speed, 0.96}],
        [{plane_id, <<"flight:83">>},{x, -116}, {y, -210}, {angle, 186}, {speed, 0.96}],
        [{plane_id, <<"flight:84">>},{x, -461}, {y, 581}, {angle, 11}, {speed, 0.47}],
        [{plane_id, <<"flight:85">>},{x, -564}, {y, 628}, {angle, 180}, {speed, 0.93}],
        [{plane_id, <<"flight:86">>},{x, -34}, {y, 24}, {angle, 66}, {speed, 0.88}],
        [{plane_id, <<"flight:87">>},{x, 687}, {y, -860}, {angle, 57}, {speed, 0.06}],
        [{plane_id, <<"flight:88">>},{x, -618}, {y, -554}, {angle, 266}, {speed, 0.93}],
        [{plane_id, <<"flight:89">>},{x, -491}, {y, -823}, {angle, 133}, {speed, 0.41}],
        [{plane_id, <<"flight:90">>},{x, -839}, {y, -851}, {angle, 228}, {speed, 0.46}],
        [{plane_id, <<"flight:91">>},{x, -332}, {y, -734}, {angle, 91}, {speed, 0.39}],
        [{plane_id, <<"flight:92">>},{x, -292}, {y, -656}, {angle, 122}, {speed, 0.19}],
        [{plane_id, <<"flight:93">>},{x, -221}, {y, 253}, {angle, 180}, {speed, 0.19}],
        [{plane_id, <<"flight:94">>},{x, 689}, {y, 820}, {angle, 72}, {speed, 0.24}],
        [{plane_id, <<"flight:95">>},{x, -38}, {y, -399}, {angle, 42}, {speed, 0.25}],
        [{plane_id, <<"flight:96">>},{x, 673}, {y, 263}, {angle, 18}, {speed, 0.31}],
        [{plane_id, <<"flight:97">>},{x, -650}, {y, 308}, {angle, 12}, {speed, 0.34}],
        [{plane_id, <<"flight:98">>},{x, -57}, {y, -686}, {angle, 41}, {speed, 0.76}],
        [{plane_id, <<"flight:99">>},{x, -169}, {y, 27}, {angle, 56}, {speed, 0.03}]
    ],
    
    lists:foreach(
        fun([{plane_id, PlaneId},{x, X},{y, Y},{angle, Angle}, {speed, Speed}]) ->
            plane:spawn_plane(PlaneId, X, Y, Angle, Speed)
        end, 
        Planes
    ).

spawn_plane(PlaneId, X, Y, Angle, Speed) ->
    sky_monitor:register_plane([{plane_id, PlaneId},{x, X},{y, Y},{angle, Angle}, {speed, Speed}]),
    plane:start([{plane_id, PlaneId},{x, X},{y, Y},{angle, Angle}, {speed, Speed}]).    

rotate_coords(V) when V < -1000 -> 2000 + V;
rotate_coords(V) when V >= 1000 -> V - 2000;
rotate_coords(V) -> V.