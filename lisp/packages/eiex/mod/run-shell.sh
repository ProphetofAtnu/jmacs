#!/bin/sh

SCRIPT=$(readlink -f "$0")
SCRIPTPATH=$(dirname "$SCRIPT")
echo $SCRIPTPATH

pushd $SCRIPTPATH

stty icanon raw
elixir --no-halt --erl "-noshell" -S mix 
stty sane

popd
