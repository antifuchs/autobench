#!/bin/bash

# recommended:
#CLISP_OPT="-q -norc -ansi -m 200MB -E latin1"
CLISP="$1"; shift

make clean optimize-files
ulimit -s 8192
"$CLISP" "$@" -i sysdep/setup-clisp.lisp -i do-compilation-script.lisp -x '(quit)'
"$CLISP" "$@" -i sysdep/setup-clisp.lisp -i do-execute-script.lisp -x '(quit)'
