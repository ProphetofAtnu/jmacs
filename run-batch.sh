#!/bin/bash

SCRIPT_DIR=`dirname $0`
BATCH_INIT_SCRIPT="$SCRIPT_DIR/batch/batch-init.el"
EMACS_BIN="emacs"

$EMACS_BIN --batch --load "$BATCH_INIT_SCRIPT" "$@"
