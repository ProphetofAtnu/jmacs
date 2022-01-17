#!/bin/sh

SCRIPTPATH=$(dirname "$0")
echo $SCRIPTPATH

pushd $SCRIPTPATH
elixir --sname "eiex@localhost" --no-halt --erl "-noshell" -S mix 
popd
