#!/usr/bin/env bash

set -e

if [ ! -d "$1" ]; then
    echo "Destination doesn't exist"
    exit 1
fi

SOURCE=$(dirname "$0")
DEST=$1

$(cd $SOURCE && wasm-pack build --target web)

mkdir -p $DEST/pkg
cp $SOURCE/pkg/sbnf_wasm.js $DEST/pkg/
cp $SOURCE/pkg/sbnf_wasm_bg.js $DEST/pkg/
cp $SOURCE/pkg/sbnf_wasm_bg.wasm $DEST/pkg/
cp $SOURCE/playground.html $DEST/
