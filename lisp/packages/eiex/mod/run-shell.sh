#!/bin/sh

SCRIPT=$(readlink -f "$0")
SCRIPTPATH=$(dirname "$SCRIPT")
echo $SCRIPTPATH

pushd $SCRIPTPATH
elixir --sname "eiex@localhost" --no-halt --erl "-noshell" -S mix 
popd
