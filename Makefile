REBAR=`which rebar || ./rebar`
ERL=erl

all: deps compile

deps:
	    @$(REBAR) get-deps

compile:
	  mkdir -p ebin 
		$(ERL) -noinput -eval "case make:all() of up_to_date -> halt(0); _ -> halt(1) end."

eunit:
	    @$(REBAR) eunit

clean:
	    @$(REBAR) clean

console:compile
	erl -pa ebin

test.spec: test.spec.in
	    cat test.spec.in | sed -e "s,@PATH@,$(PWD)," > $(PWD)/test.spec

ct: test.spec compile 
			mkdir -p logs
	    run_test -spec test.spec -pa $(PWD)/ebin -no_auto_compile -logdir $(PWD)/logs 

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
