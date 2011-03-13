ERL=erl
ERLC=erlc
APP=prittorrent

all: compile

compile:
	@$(ERL) -make

clean:
	rm -f ebin/*.beam

docs:
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' \
	'[{private, true}]'

clean-docs:
	rm -rf doc
