;;; osx.el --- My OS X specific customizations

;;; Commentary:

;; This is part of Andrew Schwartzmeyer's Emacs customizations. See
;; `init.el' for license etc.

;;; Code:

;; pull in path
(use-package exec-path-from-shell
  :if (display-graphic-p)
  :config
  (add-args-to-list 'exec-path-from-shell-variables
                    '("SSH_AUTH_SOCK" "CAML_LD_LIBRARY_PATH"))
  (exec-path-from-shell-initialize))

;; set for `shell-command-to-string' on remote systems
(setenv "TTY" "/dev/ttys001")

;; set for `shell-command-to-string' when using fish
(customize-set-variable 'shell-file-name "bash")

;; fix woman's manpath on OS X
(customize-set-variable
 'woman-manpath
 (split-string (shell-command-to-string "man --path") ":" t "\n"))

(provide 'osx)

;;; osx.el ends here
