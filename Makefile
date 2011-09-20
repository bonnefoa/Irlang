REBAR=`which rebar || ./rebar`
all: deps compile
deps:
	    @$(REBAR) get-deps
compile:
	    @$(REBAR) compile
eunit:
	    @$(REBAR) eunit
clean:
	    @$(REBAR) clean

console:compile
	erl -pa ebin

test.spec: test.spec.in
	    cat test.spec.in | sed -e "s,@PATH@,$(PWD)," > $(PWD)/test.spec

ct: test.spec src
	    run_test -pa $(PWD)/*/ebin -spec test.spec

