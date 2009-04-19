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

(setq make-backup-files nil)

;;; CL setup

(push "/opt/lisp/clbuild/source/slime" load-path)
(push "/home/autobench/.elisp" load-path)
(require 'paredit)
(require 'slime)
(slime-setup '(slime-fancy slime-autodoc slime-fancy-inspector
               slime-editing-commands))

(defun turn-on-paredit-mode ()
    (paredit-mode +1))

(add-hook 'lisp-mode-hook 'turn-on-paredit-mode)
(add-hook 'slime-repl-mode-hook 'turn-on-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-paredit-mode)

;;; RET on the REPL: Only send lines when at end of expression,
;;; newline and indent otherwise.
(defun asf-slime-repl-return (&optional end-of-input)
    (interactive "P")
      (if (= (point) (point-max))
                (slime-repl-return end-of-input)
              (paredit-newline)))

(define-key slime-repl-mode-map [(return)] 'asf-slime-repl-return)


(setq slime-port 7717)

(add-hook 'lisp-mode-hook
          (defun xach-lisp-setup ()
            (interactive)
            (xach-generic-code-setup)))

(global-set-key "\C-cs" 'slime-selector)