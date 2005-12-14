#!/usr/bin/env bash

CMUCL="$1"; shift

make clean optimize-files
"$CMUCL" "$@" -noinit -load sysdep/setup-cmucl -load do-compilation-script -eval '(ext:quit)'
"$CMUCL" "$@" -noinit -load sysdep/setup-cmucl -load do-execute-script -eval '(ext:quit)'
