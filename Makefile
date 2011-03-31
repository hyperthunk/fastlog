
REBAR=`which rebar`
all: compile

compile: precompile
	@($(REBAR) compile skip_deps=true)

precompile:
	@($(REBAR) check-deps skip_deps=true)

clean:
	@($(REBAR) clean skip_deps=true)

.PHONY: deps test compile precompile
