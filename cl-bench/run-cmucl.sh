#!/usr/bin/env bash

CMUCL="$1"; shift

make clean optimize-files
"$CMUCL" "$@" <<EOF
(load "sysdep/setup-cmucl")
(setf extensions:*gc-verbose* nil)
(load "do-compilation-script")
(ext:quit)
EOF
"$CMUCL" "$@" <<EOF
(load "sysdep/setup-cmucl")
(setf extensions:*gc-verbose* nil)
(load "do-execute-script")
(ext:quit)
EOF
