#!/bin/sh

#
#  The dumped binary calls SERVER-CONFIG2:LAUNCH-SERVICE which
#  processes the configuration file, startup options and allows the
#  user to choose which service to start.
#

sbcl --non-interactive --disable-debugger --load server-config2.lisp --eval '(progn (sb-ext:disable-debugger) (sb-ext:save-lisp-and-die "potato.bin" :toplevel #'"'"'server-config2:launch-service :executable t))'
