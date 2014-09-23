rpi_gpio
========
An Erlang module to control the GPIO pins on your RaspberryPi.

Install
=======
Just 'make' and cp the .beam files to the place you want.

Usage
=====
- export/1
- unexport/1
- set_pinmode/2
- set_pinmode/3
- set_pinattr/2
- read_pin/1
- write_pin/2
- watch_pin/2
- unwatch_pin/1

Example
=======
So far, you can do things like:

```Erlang
  -module(testdrive).

  -export([main/0]).

  main()->
    {ok, _PID} = rpi_gpio:init(),
    rpi_gpio:export(21),	% Export pin 21
    rpi_gpio:set_pinmode(21, bare),	% Set it to 'bare' mode.
    rpi_gpio:set_pinattr(21, [{direction, out}]),	% Set pin attributes
    loop(21).

  loop(Pin) ->
    loop(Pin, 1).

  loop(Pin, 0) ->
    rpi_gpio:write_pin(Pin, 1),	% 
    receive
    after 1000 ->
      loop(Pin, 1)
    end;
  loop(Pin, 1) ->
    rpi_gpio:write_pin(Pin, 0),
    receive
    after 1000 ->
      loop(Pin, 0)
    end.
```

