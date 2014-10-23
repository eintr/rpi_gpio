-module(gpio_pin).

-author("eintr<nhf0424@gmail.com>").

-export([init_allpins/0, load_pin/1, export/1, unexport/1]).

-export([pin_start/2]).

-define(GPIO_PREFIX, "/sys/class/gpio/").

init_allpins() ->
	init_pins_list(filelib:wildcard(?GPIO_PREFIX++"gpio[0-9]*"), dict:new()).

init_pins_list([], Ret) ->
	Ret;
init_pins_list([H|T], Ret) ->
	{Pin, PID} = load_pin_path(H),
	init_pins_list(T, dict:append(Pin, PID, Ret)).

load_pin(PinID) ->
	load_pin_path(?GPIO_PREFIX++"gpio"++integer_to_list(PinID)).

load_pin_path(?GPIO_PREFIX++"gpio"++Tail=Path) ->
	{list_to_integer(Tail), spawn(?MODULE, pin_start, [list_to_integer(Tail), load_pin_attribute(Path)])}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load_pin_attribute(Path) ->
	load_pin_attribute(Path, []).

load_pin_attribute(Path, Result) ->
	load_pin_attribute(Path, direction, Result).

load_pin_attribute(Path, direction, Result) ->
	case cat_file(Path++"/direction", fun utf8bin_to_atom/1) of
		{error, Reason} ->
			{error, Reason};
		{ok, Content} ->
			load_pin_attribute(Path, active_low, Result++[{direction, Content}])
	end;
load_pin_attribute(Path, active_low, Result) ->
	case cat_file(Path++"/active_low", fun binary_to_integer/1) of
		{error, Reason} ->
			{error, Reason};
		{ok, Content} ->
			load_pin_attribute(Path, edge, Result++[{active_low, Content}])
	end;
load_pin_attribute(Path, edge, Result) ->
	case cat_file(Path++"/edge", fun utf8bin_to_atom/1) of
		{error, Reason} ->
			{error, Reason};
		{ok, Content} ->
			load_pin_attribute(Path, value,  Result++[{edge, Content}])
	end;
load_pin_attribute(Path, value, Result) ->
	case cat_file(Path++"/value", fun binary_to_integer/1) of
		{error, Reason} ->
			{error, Reason};
		{ok, Content} ->
			load_pin_attribute(Path, over,  Result++[{value, Content}])
	end;
load_pin_attribute(_, over, Result) ->
	Result.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export(Pin) ->
	file:write_file(?GPIO_PREFIX++"export", integer_to_list(Pin)),
	{Pin, PID} = load_pin_path(?GPIO_PREFIX++"gpio"++integer_to_list(Pin)),
	PID.

unexport(Pin) ->
	file:write_file(?GPIO_PREFIX++"unexport", integer_to_list(Pin)).

pin_start(Pin, Context) ->
	process_flag(priority, high),
	pin_loop_start(Pin, bare, Context).

pin_loop_start(Pin, bare, Context) ->
	pin_loop(Pin, bare, Context);
pin_loop_start(Pin, pwm, Context) ->
	{ok, TimerID} = timer:send_interval(context_value(time0, Context)+context_value(time1, Context), {set0, self()}),
	pin_loop(Pin, pwm, Context++[{timerid, TimerID}]).

%%% bare loop
pin_loop(Pin, bare, Context) ->
	receive
		{set_value, From, Value} ->
			io:format("Pin[~p]: {set_value, ~p}\n", [Pin, Value]),
			case context_value(direction, Context) of
				in ->
					From ! {error, "pin was defined input."};
				out ->
					set_pinvalue(Pin, Value),
					From ! ok,
					pin_loop(Pin, bare, lists:keyreplace(value, 1, Context, {value, Value}));
				_ ->
					From ! {error, "Unknown error: No 'direction' value for pin."}
			end,
			pin_loop(Pin, bare, Context);
		{set_attrs, From, NewAttrs} ->
			io:format("Pin[~p]: {set_attr, ~p}\n", [Pin, NewAttrs]),
			NewContext = update_attr(Pin, Context, NewAttrs),
			From ! ok,      % BUG: Racing!
			pin_loop_start(Pin, context_value(mode, NewAttrs), NewContext);
		CommonMsg ->
			pin_loop_common_msg_handle(Pin, Context, CommonMsg),
			pin_loop(Pin, bare, Context)
	end;

%%% PWM loop
pin_loop(Pin, pwm, Context) ->
	receive
		{set1, _} ->
			set_pinvalue(Pin, 1),
			timer:send_after(context_value(time1, {set0, self()})),
			pin_loop(Pin, pwm, Context);
		{set0, _} ->
			set_pinvalue(Pin, 0),
			pin_loop(Pin, pwm, Context);
		{set_attrs, From, NewAttrs} ->
			io:format("Pin[~p]: {set_attr, ~p}\n", [Pin, NewAttrs]),
			NewContext = update_attr(Pin, Context, NewAttrs),
			From ! ok,      % BUG: Racing!
			NewMode = context_value(mode, NewAttrs),
			if
				NewMode =/= pwm ->
					timer:cancel(context_value(timerid, Context)),
					NewContext = lists:keydelete(timerid, 1, Context),
					pin_loop_start(Pin, context_value(mode, NewAttrs), NewContext);
				true ->
					pin_loop(Pin, pwm, NewContext)
			end;
		CommonMsg ->
			pin_loop_common_msg_handle(Pin, Context, CommonMsg),
			pin_loop(Pin, pwm, Context)
	end;

%%% Dumb loop
pin_loop(Pin, Mode, Context) ->
	receive
		{set_attrs, From, NewAttrs} ->
			io:format("Pin[~p]: {set_attr, ~p}\n", [Pin, NewAttrs]),
			NewContext = update_attr(Pin, Context, NewAttrs),
			From ! ok,      % BUG: Racing!
			pin_loop_start(Pin, context_value(mode, NewAttrs), NewContext);
		{_, From} ->
			From ! {error, "Pin mode unknown."},
			pin_loop(Pin, Mode, Context)
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

update_attr(Pin, Map, []) ->
	io:format("Set attr pin[~p] -> ~p~n", [Pin, Map]),
	maps:fold(	fun (K, V, _AccIN) when is_integer(V) -> 
				%io:format("~p <= ~p~n", [?GPIO_PREFIX++"gpio"++integer_to_list(Pin)++"/"++atom_to_list(K), integer_to_list(V)]),
				file:write_file(?GPIO_PREFIX++"gpio"++integer_to_list(Pin)++"/"++atom_to_list(K), integer_to_list(V));
			(K, V, _AccIN) when is_atom(V) -> 
				%io:format("~p <= ~p~n", [?GPIO_PREFIX++"gpio"++integer_to_list(Pin)++"/"++atom_to_list(K), atom_to_list(V)]),
				file:write_file(?GPIO_PREFIX++"gpio"++integer_to_list(Pin)++"/"++atom_to_list(K), atom_to_list(V)) end,
	[], Map),
	Map;
update_attr(Pin, Map, [{K, V}|T]) ->
	update_attr(Pin, maps:put(K, V, Map), T).

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

context_value(Key, Context) ->
	case lists:keysearch(Key, 1, Context) of
		{Key, Value} ->
			Value;
		false ->
			false
	end.

