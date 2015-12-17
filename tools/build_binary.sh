#!/bin/sh

sbcl --non-interactive --disable-debugger --load server-config2.lisp --eval '(progn (sb-ext:disable-debugger) (sb-ext:save-lisp-and-die "potato.bin" :toplevel #'"'"'server-config2:launch-service :executable t))'
