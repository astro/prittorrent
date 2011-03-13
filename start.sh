#!/bin/sh

ENV=""
[ "$1" ] && ENV="-servtorrent seedlist \"$1\""
[ "$2" ] && ENV="$ENV -servtorrent wire_port $2"

erl -pa ebin -make && \
erl -config sasl +K true -smp auto \
    -pa ebin \
    -s servtorrent \
    $ENV
