#!/bin/sh

set -e

DEST=../public/assets/vendor/
if [ ! -d $DEST ] && [ -d ../public ]; then
	mkdir -p $DEST
fi

echo "Copy vendor files"
cp -r src/vendor/* $DEST
