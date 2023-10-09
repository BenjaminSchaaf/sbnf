#!/usr/bin/env sh

ARCH=$(arch)

if [ "$ARCH" = "x86_64" ]; then
    "$(dirname -- "$0")/sbnf-x86_64-unknown-linux-gnu" "$@"
elif [ "$ARCH" = "aarch64" ]; then
    "$(dirname -- "$0")/sbnf-aarch64-unknown-linux-gnu" "$@"
else
    echo "Unrecognized architecture '$ARCH'" >&2
    exit 1
fi
