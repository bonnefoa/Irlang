REBAR=`which rebar || ./rebar`
all: deps compile
deps:
	    @$(REBAR) get-deps
compile:
	    @$(REBAR) compile
test:
	    @$(REBAR) eunit
clean:
	    @$(REBAR) clean

console:compile
	erl -pa ebin
