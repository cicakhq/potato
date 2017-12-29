#!/bin/sh

set -e

DEST=../public/assets/img/
if [ ! -d $DEST ] && [ -d ../public ]; then
        mkdir -p $DEST
fi

echo "Copy images"
cp -r src/images/* $DEST
