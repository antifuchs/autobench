;;; playing with the CPU Performance Counters on CMUCL for Solaris
;;;
;;; Time-stamp: <2003-10-09 emarsden>

#-sparc-v9
(error "Performance counters are only present on an UltraSPARC")

(unless
    (ignore-errors (require :cpc))
  (error "Can't load CPC subsystem"))

(setq ext:*gc-verbose* nil)

(push :performance-counters *features*)


(load "defpackage")
(compile-file "sysdep/setup-cmucl" :load t)
(load "do-compilation-script")
(load "do-execute-script")

;; EOF
