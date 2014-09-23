-module(rpi_gpio_pin).

-author("eintr<nhf0424@gmail.com>").

-export([init_allpins/0, init_pin/1, export/1, unexport/1]).

-export([pin_loop_start/2]).

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
	{list_to_integer(Tail), spawn(?MODULE, pin_loop_start, [list_to_integer(Tail), init_pin_context(Path)])}.

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
	file:write_file(?GPIO_PREFIX++"export", integer_to_list(Pin)),
	{Pin, PID} = init_pin_path(?GPIO_PREFIX++"gpio"++integer_to_list(Pin)),
	PID.

unexport(Pin) ->
	file:write_file(?GPIO_PREFIX++"unexport", integer_to_list(Pin)).

pin_loop_start(Pin, Context) ->
	process_flag(priority, high),
	pin_loop(Pin, Context).

%%% bear loop
pin_loop(Pin, #pin_context{mode=bare, attribute=Attr, value=OldValue}=Context) ->
	receive
		{set_value, From, OldValue} ->
			io:format("Pin[~p]: {set_value, ~p}\n", [Pin, OldValue]),
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
			io:format("Pin[~p]: {set_value, ~p}\n", [Pin, Value]),
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
		{change_attr, From, NewAttr} ->
			io:format("Pin[~p]: {change_attr, ~p}\n", [Pin, NewAttr]),
			From ! ok,      % BUG: Racing!
			pin_loop(Pin, #pin_context{mode=bare, attribute=update_attr(Attr, NewAttr), value=OldValue});
		{change_mode, From, NewMode} ->
			io:format("Pin[~p]: {change_mode, ~p}\n", [Pin, NewMode]),
			set_pinvalue(Pin, 0),
			From ! ok,	% BUG: Racing!
			pin_loop(Pin, #pin_context{mode=NewMode, attribute=Attr, value=0});
		{change_mode, From, NewMode, NewAttr} ->
			io:format("Pin[~p]: {change_mode, ~p, ~p}\n", [Pin, NewMode, NewAttr]),
			set_pinvalue(Pin, 0),
			From ! ok,	% BUG: Racing!
			pin_loop(Pin, #pin_context{mode=NewMode, attribute=update_attr(Attr, NewAttr), value=0});
		CommonMsg ->
			pin_loop_common_msg_handle(Pin, Context, CommonMsg),
			pin_loop(Pin, Context)
	end;

%%% PWM loop
pin_loop(Pin, #pin_context{mode=pwm, attribute=Attr, value=0}=Context) ->
	{ok, T0} = maps:find(time0, Attr),
	receive
		{change_mode, From, NewMode, NewAttr} ->
			set_pinvalue(Pin, 0),
			From ! ok,	% BUG: Racing!
			pin_loop(Pin, #pin_context{mode=NewMode, attribute=update_attr(Attr, NewAttr), value=0});
		CommonMsg ->
			pin_loop_common_msg_handle(Pin, Context, CommonMsg),
			pin_loop(Pin, Context)
	after T0 ->
		set_pinvalue(Pin, 1),
		pin_loop(Pin, #pin_context{mode=pwm, attribute=Attr, value=1})
	end;
pin_loop(Pin, #pin_context{mode=pwm, attribute=Attr, value=1}=Context) ->
	{ok, T1} = maps:find(time1, Attr),
	receive
		{change_mode, From, NewMode, NewAttr} ->
			set_pinvalue(Pin, 0),
			From ! ok,	% BUG: Racing!
			pin_loop(Pin, #pin_context{mode=NewMode, attribute=update_attr(Attr, NewAttr), value=0});
		CommonMsg ->
			pin_loop_common_msg_handle(Pin, Context, CommonMsg),
			pin_loop(Pin, Context)
	after T1 ->
		set_pinvalue(Pin, 0),
		pin_loop(Pin, #pin_context{mode=pwm, attribute=Attr, value=0})
	end;

%%% Dumb loop
pin_loop(Pin, #pin_context{mode=_, attribute=_, value=_}=Context) ->
	receive
		{_, From} ->
			From ! {error, "Pin mode unknown."},
			pin_loop(Pin, Context)
	end.

pin_loop_common_msg_handle(_Pin, Context, Msg) ->
	case Msg of
		{'EXIT', From} ->
			From ! {error, "Function not implemented yet."},
			exit("NoRespawn");
		{status, From} ->
			From ! Context;
		{Code, From} ->
			From ! {error, "Can't understand messasge code: "++atom_to_list(Code)}
	end.

update_attr(Map, []) ->
	Map;
update_attr(Map, [{K, V}|T]) ->
	update_attr(maps:put(K, V, Map), T).

set_pinvalue(Pin, 0) ->
	ok = file:write_file(?GPIO_PREFIX++"gpio"++integer_to_list(Pin)++"/value", "0");
set_pinvalue(Pin, _) ->
	ok = file:write_file(?GPIO_PREFIX++"gpio"++integer_to_list(Pin)++"/value", "1").

cat_file(Path, Converter) ->
	{ok, LineBin} = file:read_file(Path),
	[H|_] = string:tokens(binary_to_list(LineBin), " \n"),
	{ok, Converter(list_to_binary(H))}.

utf8bin_to_atom(B) ->
	binary_to_atom(B, utf8).

