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

ct: test.spec compile 
			mkdir -p log
	    run_test -spec test.spec -pa $(PWD)/ebin 

build_plt:
	dialyzer --build_plt                 \
		--output_plt .dialyzer-R14B01.plt  \
		--apps kernel stdlib sasl erts ssl \
		tools os_mon runtime_tools crypto  \
	inets xmerl webtool snmp public_key  \
	mnesia eunit syntax_tools compiler   

check_plt:build_plt
	dialyzer ./ebin --plt .dialyzer-R14B \
	-Wunmatched_returns                  \
		-Werror_handling                   \
		-Wrace_conditions                  \
		-Wbehaviours                       \
		-Wunderspecs
