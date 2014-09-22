sources=rpi_gpio.erl rpi_gpio_pin.erl

beams=$(sources:.erl=.beam)

all: $(beams)

%.beam: %.erl
	erlc $^

install: get-deps all
	cp $(beams) ../dist/ebin/
	cp titans_id_service ../dist/ebin/
	cp titans_id_script ../dist/ebin/

clean:
	rm -f $(beams)

get-deps:
	echo No deps needed.

