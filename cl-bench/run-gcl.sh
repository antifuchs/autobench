#!/bin/sh

GCL_ANSI=1
export GCL_ANSI
GCL=gclcvs

${GCL} -load sysdep/setup-gcl.lisp -load do-compilation-script.lisp -eval '(quit)'
${GCL} -load sysdep/setup-gcl.lisp -load do-execute-script.lisp -eval '(quit)'
