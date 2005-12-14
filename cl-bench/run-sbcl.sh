#!/bin/bash

SBCL="$1"; shift

make clean optimize-files
"$SBCL" "$@" <<EOF
 (progn 
   (load "sysdep/setup-sbcl.lisp") 
   (load "do-compilation-script.lisp") 
   (sb-ext:quit))
EOF

"$SBCL" "$@" <<EOF 
 (progn 
   (load "sysdep/setup-sbcl.lisp") 
   (load "do-execute-script.lisp") 
   (sb-ext:quit))
EOF
