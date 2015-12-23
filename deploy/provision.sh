#!/bin/sh

SBCL_PATH=""
OS=$(uname -s)

if [ "$OS" == "FreeBSD" ]; then
    /usr/sbin/pkg install -y sbcl
    SBCL_PATH="/usr/local/bin/sbcl"
fi

if [ -n "$SBCL_PATH" ] && [ -x "$SBCL_PATH" ]; then
    exec $SBCL_PATH --script "/var/tmp/provision.cl"
else
    >&2 echo "$SBCL_PATH is not executable ($OS)"
    exit 1
fi

