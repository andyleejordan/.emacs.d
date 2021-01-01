;;; osx.el --- My OS X specific customizations

;;; Commentary:

;; This is part of Andrew Schwartzmeyer's Emacs customizations. See
;; `init.el' for license etc.

;;; Code:

;; set for `shell-command-to-string' on remote systems
(setenv "TTY" "/dev/ttys001")

;; set for `shell-command-to-string' when using fish
(customize-set-variable 'shell-file-name "bash")

;; fix woman's manpath on OS X
(customize-set-variable
 'woman-manpath
 (split-string (shell-command-to-string "man --path") ":" t "\n"))

(cd "~/")

(provide 'osx)

;;; osx.el ends here
