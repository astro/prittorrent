ERL=erl
ERLC=erlc
APP_TITLE=PritTorrent
APP=servtorrent

all: compile

compile:
	@$(ERL) -make

clean:
	rm -f ebin/*.beam

docs:
	@$(ERL) -noshell -run edoc_run application '$(APP_TITLE)' '"."' \
	'[{private, true}]'

clean-docs:
	rm -rf doc

test: compile
	@$(ERL) -pa ebin -eval "eunit:test({application,$(APP)})" \
	-noshell -s init stop
