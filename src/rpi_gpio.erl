-module(rpi_gpio).

-author("eintr<nhf0424@gmail.com>").

-define(SERVERNAME, gpio_server).

-export([init/0, set_pinmode/2, get_pinmode/1, read_pin/1, write_pin/2, watch_pin/2, unwatch_pin/1, status/1, status/0]).

-export([server_start/0]).

init() ->
	case whereis(?SERVERNAME) of
		undefined ->
			{ok, register(?SERVERNAME, spawn(?MODULE, server_start, []))};
		_Pid ->
			{error, "init() is unnessesary, already started."}
	end.

server_start() ->
	server_loop(rpi_gpio_pin:init_pins()).

server_loop(PinPIDMap) ->
	receive
		{exit, From} ->
			rpi_gpio_pin:exit(),
			From ! ok;
		{refresh, From} ->
			From ! ok,
			server_loop(rpi_gpio_pin:init_pins());
		{get, From, PinID} ->
			From ! maps:find(PinID, PinPIDMap),
			server_loop(PinPIDMap);
		{getmap, From} ->
			From ! PinPIDMap,
			server_loop(PinPIDMap);
		{'EXIT', PID, _Reason} ->
			PinID = pid_to_pin(PID, PinPIDMap),
			server_loop(maps:put(PinID, rpi_gpio_pin:init_pin(PinID), PinPIDMap))
	end.

pid_to_pin(PID, PinPIDMap) ->
	maps:fold(fun (K, P, P) -> K;(_,_,AccIn) ->AccIn end, PID, PinPIDMap).

set_pinmode(PinID, Mode) ->
	msg_relay_pin(PinID, {set_pinmode, self(), PinID, Mode}).

get_pinmode(PinID) ->
	msg_relay_pin(PinID, {get_pinmode, self(), PinID}).

read_pin(PinID) ->
	msg_relay_pin(PinID, {read_pin, self(), PinID}).

write_pin(PinID, Value) ->
	msg_relay_pin(PinID, {write_pin, self(), PinID, Value}).

watch_pin(PinID, Func) ->
	msg_relay_pin(PinID, {watch_pin, self(), PinID, Func}).

unwatch_pin(PinID) ->
	msg_relay_pin(PinID, {unwatch_pin, self(), PinID}).

status(PinID) ->
	msg_relay_pin(PinID, {status, self(), PinID}).

status() ->
	?SERVERNAME ! {getmap, self()},
	receive
		Map ->
			maps:map(fun(PinID, PID)->msg_relay_pid(PID, {status, self(), PinID}) end, Map)
	end.

msg_relay_pin(PinID, Msg) ->
	?SERVERNAME ! {get, PinID},
	receive
		{ok, PID} ->
			msg_relay_pid(PID, Msg);
		error ->
			{error, "PinID not administratred."}
	end.

msg_relay_pid(Pid, Msg) ->
	Pid ! Msg,
	receive
		Ret -> Ret
	end.

