#!/bin/bash

SBCL=${SBCL:-sbcl}

make clean optimize-files
$SBCL ${SBCL_OPT} <<EOF
 (progn 
   (load "sysdep/setup-sbcl.lisp") 
   (load "do-compilation-script.lisp") 
   (sb-ext:quit))
EOF

$SBCL ${SBCL_OPT} <<EOF 
 (progn 
   (load "sysdep/setup-sbcl.lisp") 
   (load "do-execute-script.lisp") 
   (sb-ext:quit))
EOF
