-module(rpi_gpio_pin).

-author("eintr<nhf0424@gmail.com>").

-export([init_allpins/0, init_pin/1, export/1, unexport/1]).

-export([pin_loop/2]).

-define(GPIO_PREFIX, "/sys/class/gpio/").

-record( pin_context, {
		mode = bare,
		attribute, % is a map
		value
	}
).

init_allpins() ->
	init_pins_list(filelib:wildcard(?GPIO_PREFIX++"gpio[0-9]*"), maps:new()).

init_pins_list([], Ret) ->
	Ret;
init_pins_list([H|T], Ret) ->
	{Pin, PID} = init_pin_path(H),
	init_pins_list(T, maps:put(Pin, PID, Ret)).

init_pin(PinID) ->
	init_pin_path(?GPIO_PREFIX++"gpio"++integer_to_list(PinID)).

init_pin_path(?GPIO_PREFIX++"gpio"++Tail=Path) ->
	{list_to_integer(Tail), spawn(?MODULE, pin_loop, [list_to_integer(Tail), init_pin_context(Path)])}.

init_pin_context(Dir) ->
	#pin_context{mode=bare, attribute=init_pin_attribute(Dir, maps:new()), value=cat_file(Dir++"/value", fun binary_to_integer/1)}.

init_pin_attribute(Path, Result) ->
	init_pin_attribute(Path, direction, Result).

init_pin_attribute(Path, direction, Result) ->
	case cat_file(Path++"/direction", fun utf8bin_to_atom/1) of
		{error, Reason} ->
			{error, Reason};
		{ok, Content} ->
			init_pin_attribute(Path, active_low, maps:put(direction, Content, Result))
	end;
init_pin_attribute(Path, active_low, Result) ->
	case cat_file(Path++"/active_low", fun binary_to_integer/1) of
		{error, Reason} ->
			{error, Reason};
		{ok, Content} ->
			init_pin_attribute(Path, edge, maps:put(active_low, Content, Result))
	end;
init_pin_attribute(Path, edge, Result) ->
	case cat_file(Path++"/edge", fun utf8bin_to_atom/1) of
		{error, Reason} ->
			{error, Reason};
		{ok, Content} ->
			init_pin_attribute(Path, over,  maps:put(edge, Content, Result))
	end;
init_pin_attribute(_, over, Result) ->
	Result.

export(Pin) ->
    {ok, IO} = file:open(?GPIO_PREFIX++"export", [write]),
    io:format(IO, "~p~n", [Pin]),
	{Pin, PID} = init_pin_path(?GPIO_PREFIX++"gpio"++integer_to_list(Pin)),
	PID.

unexport(Pin) ->
	IO = file:open(?GPIO_PREFIX++"unexport", [write]),
	io:format(IO, "~p~n", [Pin]),
	ok.

pin_loop(Pin, #pin_context{mode=bare, attribute=Attr, value=OldValue}=Context) ->
	receive
		{set_value, From, OldValue} ->
			case maps:find(direction, Attr) of
				{ok, in} ->
					From ! {error, "pin was defined input."};
				{ok, out} ->
					From ! ok;
				_ ->
					From ! {error, "Unknown error: No 'direction' value for pin."}
			end,
			pin_loop(Pin, Context);
		{set_value, From, Value} ->
			case maps:find(direction, Attr) of
				{ok, in} ->
					From ! {error, "pin was defined input."};
				{ok, out} ->
					set_pinvalue(Pin, Value),
					From ! ok,
					pin_loop(Pin, #pin_context{mode=bare, attribute=Attr, value=Value});
				_ ->
					From ! {error, "Unknown error: No 'direction' value for pin."}
			end,
			pin_loop(Pin, Context);
		{change_mode, From, NewMode, NewAttr} ->
			set_pinvalue(Pin, 0),
			From ! ok,	% BUG: Racing!
			pin_loop(Pin, #pin_context{mode=NewMode, attribute=update_attr(Attr, NewAttr), value=0});
		{_, From} ->
			From ! {error, "Function not implemented yet."},
			pin_loop(Pin, Context)
	end;
pin_loop(Pin, #pin_context{mode=pwm, attribute=Attr, value=0}) ->
	{ok, T0} = maps:find(time0, Attr),
	receive
		{change_mode, From, NewMode, NewAttr} ->
			set_pinvalue(Pin, 0),
			From ! ok,	% BUG: Racing!
			pin_loop(Pin, #pin_context{mode=NewMode, attribute=update_attr(Attr, NewAttr), value=0})
	after T0 ->
		set_pinvalue(Pin, 1),
		pin_loop(Pin, #pin_context{mode=pwm, attribute=Attr, value=1})
	end;
pin_loop(Pin, #pin_context{mode=pwm, attribute=Attr, value=1}) ->
	{ok, T1} = maps:find(time1, Attr),
	receive
		{change_mode, From, NewMode, NewAttr} ->
			set_pinvalue(Pin, 0),
			From ! ok,	% BUG: Racing!
			pin_loop(Pin, #pin_context{mode=NewMode, attribute=update_attr(Attr, NewAttr), value=0})
	after T1 ->
		set_pinvalue(Pin, 0),
		pin_loop(Pin, #pin_context{mode=pwm, attribute=Attr, value=0})
	end;

pin_loop(Pin, #pin_context{mode=_, attribute=_, value=_}=Context) ->
	receive
		{_, From} ->
			From ! {error, "Pin mode unknown."},
			pin_loop(Pin, Context)
	end.

update_attr(Map, []) ->
	Map;
update_attr(Map, [{K, V}|T]) ->
	update_attr(maps:put(K, V, Map), T).

set_pinvalue(Pin, Value) ->
	{ok, IO} = file:open(?GPIO_PREFIX++"gpio"++integer_to_list(Pin), [write]),
	io:format(IO, "~p~n", [Value]).

cat_file(Path, Converter) ->
	{ok, Io} =file:open(Path, [read]),
	{ok, Line} = file:read_line(Io),
	[H|_] = string:tokens(Line, " \n"),
	{ok, Converter(list_to_binary(H))}.

utf8bin_to_atom(B) ->
	binary_to_atom(B, utf8).

