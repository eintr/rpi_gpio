sources=gpio.erl gpio_pin.erl

beams=$(sources:.erl=.beam)

all: $(beams)

%.beam: %.erl
	erlc $^

install: get-deps all
	cp $(beams) ../dist/ebin/

clean:
	rm -f $(beams)

get-deps:
	echo No deps needed.

