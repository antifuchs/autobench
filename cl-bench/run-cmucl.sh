#!/bin/bash

CMUCL=${CMUCL:-"cmucl-latest"}
CMUCLOPT=${CMUCLOPT:-""}

make clean optimize-files
$CMUCL $CMUCLOPT -noinit -load sysdep/setup-cmucl -load do-compilation-script -eval '(ext:quit)'
$CMUCL $CMUCLOPT -noinit -load sysdep/setup-cmucl -load do-execute-script -eval '(ext:quit)'
