REBAR ?= rebar3

all: dirs compile

dirs:
	@mkdir -p priv/tmp

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

dialyzer:
	@$(REBAR) dialyzer

typer:
	@typer -pa _build/default/lib/wierl/ebin -I include \
		--plt _build/default/*_plt -r ./src
