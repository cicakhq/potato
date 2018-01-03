#!/bin/sh

set -e

DEST=../public/assets/html/
if [ ! -d $DEST ] && [ -d ../public ]; then
	mkdir -p $DEST
fi

echo "Copy HTML files"
cp -r src/html/* $DEST
