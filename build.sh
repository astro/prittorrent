#!/usr/bin/env bash -e

for app in feeds hasher seeder ; do
    rebar3 generate -n $app
    mkdir -p _build/default/rel/$app/log
done
