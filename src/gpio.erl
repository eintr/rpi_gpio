-module(gpio).

-author("eintr<nhf0424@gmail.com>").

-define(SERVERNAME, gpio_server).

-export([init/0, export/1, unexport/1, set_pinmode/2, set_pinmode/3, set_pinattr/2, get_pinmode/1, read_pin/1, write_pin/2, watch_pin/2, unwatch_pin/1, status/1, status/0]).

-export([server_start/0]).

init() ->
	case whereis(?SERVERNAME) of
		undefined ->
			{ok, register(?SERVERNAME, spawn(?MODULE, server_start, []))};
		_Pid ->
			{error, "init() is unnessesary, already started."}
	end.

server_start() ->
	server_loop(rpi_gpio_pin:init_allpins()).

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
		{export, From, PinID} ->
			case rpi_gpio_pin:export(PinID) of
				{error, Reason} ->
					From ! {error, Reason};
				PID ->
					From ! PID,
					server_loop(maps:put(PinID, PID, PinPIDMap))
			end;
		{unexport, From, PinID} ->
			case rpi_gpio_pin:unexport(PinID) of
				Ret ->
					From ! {error, Ret}
			end;
		{getmap, From} ->
			From ! PinPIDMap,
			server_loop(PinPIDMap);
		{'EXIT', PID, _Reason} ->
			PinID = pid_to_pin(PID, PinPIDMap),
			server_loop(maps:put(PinID, rpi_gpio_pin:init_pin(PinID), PinPIDMap))
	end.

pid_to_pin(PID, PinPIDMap) ->
	maps:fold(fun (K, P, P) -> K;(_,_,AccIn) ->AccIn end, PID, PinPIDMap).

export(Pin) ->
	msg_relay_pid(?SERVERNAME, {export, self(), Pin}).

unexport(Pin) ->
	msg_relay_pid(?SERVERNAME, {unexport, self(), Pin}).

set_pinmode(PinID, Mode) ->
	msg_relay_pin(PinID, {change_mode, self(), Mode}).

set_pinmode(PinID, Mode, Attr) ->
	msg_relay_pin(PinID, {change_mode, self(), Mode, Attr}).

set_pinattr(PinID, Attr) ->
	msg_relay_pin(PinID, {change_attr, self(), Attr}).

get_pinmode(PinID) ->
	msg_relay_pin(PinID, {get_pinmode, self()}).

read_pin(PinID) ->
	msg_relay_pin(PinID, {read_pin, self()}).

write_pin(PinID, Value) ->
	msg_relay_pin(PinID, {set_value, self(), Value}).

watch_pin(PinID, Func) ->
	msg_relay_pin(PinID, {watch_pin, self(), Func}).

unwatch_pin(PinID) ->
	msg_relay_pin(PinID, {unwatch_pin, self()}).

status(PinID) ->
	msg_relay_pin(PinID, {status, self()}).

status() ->
	?SERVERNAME ! {getmap, self()},
	receive
		Map ->
			maps:map(fun(PinID, PID)->msg_relay_pid(PID, {status, self(), PinID}) end, Map)
	end.

msg_relay_pin(PinID, Msg) ->
	?SERVERNAME ! {get, self(), PinID},
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

