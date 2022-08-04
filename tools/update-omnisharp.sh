#!/bin/bash

REL_URL='https://api.github.com/repos/OmniSharp/omnisharp-roslyn/releases/latest'
TMP_DIR=`mktemp -d`
LINUX_TARFILE='omnisharp-linux-x64-net6.0.tar.gz'
LATEST="$TMP_DIR/latest.json"
OUTFILE="$TMP_DIR/latest.tar.gz"

LSP_INSTALL_DIR="$HOME/.emacs.d/var/lsp/server/omnisharp-roslyn/latest/omnisharp-roslyn/"

curl $REL_URL -o $TMP_DIR/latest.json

DL_URL=`cat $LATEST |\
    jq -r  '.assets[] | select(.name == "omnisharp-linux-x64-net6.0.tar.gz") | .browser_download_url'`

echo $DL_URL

curl "$DL_URL" -L -o $OUTFILE

rm -rf "$LSP_INSTALL_DIR/*"
tar -C "$LSP_INSTALL_DIR" -xzf "$OUTFILE"


rm -rf $TMP_DIR
echo "Cleaned up TMP dir"
