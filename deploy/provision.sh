#!/bin/sh

SBCL_PATH=""
OS=$(uname -s)

POTATO_ROOT_FOLDER="$1"
POTATO_LISP_SCRIPT="$2"
POTATO_FULL_SCRIPT="$POTATO_ROOT_FOLDER/$POTATO_LISP_SCRIPT"

if [ "$OS" == "FreeBSD" ]; then
    echo "Installing SBCL"
    /usr/sbin/pkg install -q -y sbcl
    SBCL_PATH="/usr/local/bin/sbcl"
fi

if [ -f "$POTATO_FULL_SCRIPT" ]; then
    if [ -n "$SBCL_PATH" ] && [ -x "$SBCL_PATH" ]; then
        exec $SBCL_PATH --script "$POTATO_FULL_SCRIPT"
    else
        >&2 echo "$SBCL_PATH is not executable ($OS)."
        exit 1
    fi
else
    >&2 echo "$POTATO_FULL_SCRIPT wasn't found."
    exit 1
fi

