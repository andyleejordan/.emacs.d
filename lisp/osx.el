;;; osx --- OS X specific configurations

;;; Commentary:

;;; Code:

;; pull in path
(use-package "exec-path-from-shell"
  :if (display-graphic-p)
  :config
  (add-to-list 'exec-path-from-shell-variables "SSH_AUTH_SOCK")
  (exec-path-from-shell-initialize))

(if (display-graphic-p)
    (set-face-attribute 'default nil :font "Hack"))

;; set for shell-command-to-string on remote systems
(setenv "TTY" "/dev/ttys001")

;; fix woman's manpath on OS X
(setq woman-manpath
      (split-string (shell-command-to-string "man --path") ":" t "\n"))

;; delete by moving to trash
(setq delete-by-moving-to-trash t)

;; open file's location in Finder
(defun finder ()
  "Opens file directory in Finder."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (shell-command
         (format "%s %s" (executable-find "open") (file-name-directory file)))
      (error "Buffer is not attached to any file"))))

;;; provide OS X package
(provide 'osx)

;;; osx.el ends here
