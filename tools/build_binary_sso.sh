#!/bin/sh

sbcl --non-interactive --disable-debugger --load server-config2.lisp --eval '(progn (sb-ext:disable-debugger) (ql:quickload "potato-sso") (sb-ext:save-lisp-and-die "potato.bin" :toplevel #'"'"'server-config2:launch-service :executable t))'
