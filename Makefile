all:
	mkdir -p ebin
	erlc -o ebin -I include src/uberpt.erl
	erlc -pa ebin -o ebin -I include src/*.erl
