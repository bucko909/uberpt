all:
	mkdir -p ebin
	erlc +debug_info -o ebin -I include src/uberpt.erl
	erlc +debug_info -pa ebin -o ebin -I include src/*.erl

test:
	erl -noshell -pa ebin -eval "eunit:test(uberpt, [verbose])" -s init stop
