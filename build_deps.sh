#!/bin/bash
pushd deps/jsx && ../../rebar compile && popd
pushd deps/wrangler && ./configure && make && popd
