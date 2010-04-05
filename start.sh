#!/bin/sh

erl -make && \
erl +K true -smp auto \
    -pa ebin \
    -s servtorrent
