-module(rpi_gpio).

-author().

-define(SERVERNAME, gpio_server)

-export([init/0, export/1, unexport/1, config/2, read/1, write/2]).

-record( pin_status, {
		active_low=0,
		direction=in,
		edge=none,
		power,
		uevent,
		value=0
	}
).

-record( gpio_context, {
		exported,
	}
).

init() ->
	case whereis(?SERVERNAME) of
		undefined ->
			{ok, register(?SERVERNAME, spawn(?MODULE, server_start, []))};
		Pid ->
			{error, "Server already started."}
	end.

server_start() ->
	server_loop(accuire_pins()).

accuire_pins() ->
	accuire_pins(filelib:wildcard("/sys/class/gpio/gpio[0-9]*"), []).

accuire_pins([], Ret) ->
	Ret;
accuire_pins([H|T], Ret) ->
	accuire_pins(T, Ret++[accuire_1pin(H)]).

accuire_1pin("/sys/class/gpio/gpio"++Tail=Path) ->
	{binary_to_integer(Tail), #pin_status	{
			active_low = cat_file(Path++"/active_low", fun binary_to_integer),
			direction = cat_file(Path++"/direction", fun binary_to_atom),
			edge = cat_file(Path++"/edge", fun binary_to_atom),
			power = unsupported,
			uevent = unsupported,
			value = cat_file(Path++"/value", fun binary_to_integer)
		}
	}

cat_file(Path, Converter) ->
	Io=file:open(Path, [read, binary]),
	{ok, LineBin} = file:read_line(Io),
	Converter(LineBin).

server_loop(Context)
	receive
		{exit, From} ->
			From ! ok;
		{refresh, From} ->
			From ! ok,
			server_loop(accuire_pins());
		{}
	end.

