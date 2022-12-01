#!/bin/bash 
#
# This script assumes that you also use neovim and the nvim-treesitter plugin.
# Copy and rename all the so files that nvim is nice enough to collect and update.

EMACS_DIR="$HOME/.emacs.d/tree-sitter"

for tslib in $(find $HOME/.local/share/nvim -type f \( -iwholename "*nvim-treesitter/parser/*.so" -o -iwholename "*nvim-treesitter/parser/*.dylib" \)); do 
  elname=`basename $tslib`
  cp -f $tslib $EMACS_DIR/libtree-sitter-$elname;
done
