(require 'cl)

;;; elisp setup

(defun xach-generic-code-setup ()
  (setq indent-tabs-mode nil)
  (local-set-key "\C-m" 'newline-and-indent))

(add-hook 'emacs-lisp-mode-hook
	  (defun xach-emacs-setup ()
	    (interactive)
	    (xach-generic-code-setup)
	    (eldoc-mode t)))

(setq indent-tabs-mode nil)

;;; CL setup

(push "/opt/lisp/clbuild/source/slime" load-path)
(require 'slime)
(slime-setup '(slime-fancy slime-autodoc slime-fancy-inspector
               slime-editing-commands))
(setq slime-port 7717)

(add-hook 'lisp-mode-hook
          (defun xach-lisp-setup ()
            (interactive)
            (xach-generic-code-setup)))

(global-set-key "\C-cs" 'slime-selector)