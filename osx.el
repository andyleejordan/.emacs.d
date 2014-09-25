;;; osx --- OS X specific configurations

;;; Commentary:
;;; Uses Source Code Pro font
;;; http://sourceforge.net/projects/sourcecodepro.adobe/

;;; Code:

;; pull in environment
(setenv "SHELL" "/usr/local/bin/bash")
(exec-path-from-shell-copy-envs '("GPG_AGENT_INFO"))
(exec-path-from-shell-initialize)

;; add home info manuals
(add-to-list 'Info-additional-directory-list (expand-file-name "~/info"))

;; key bindings
(bind-key "C-c d" 'dash-at-point)

;; disable toolbar and scrollbar
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; frame size
(add-to-list 'default-frame-alist '(height . 44))
(add-to-list 'default-frame-alist '(width . 88))

;; delete by moving to trash
(setq delete-by-moving-to-trash t)

;; set font
(set-frame-font
 "-apple-Source_Code_Pro-medium-normal-normal-*-*-*-*-*-m-0-iso10646-1" nil t)

;; font size
(set-face-attribute 'default nil :height 120)

;; use srgb
(setq ns-use-srgb-colorspace t)

;; open file's location in Finder
(defun finder ()
  "Opens file directory in Finder."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (shell-command
         (format "%s %s" (executable-find "open") (file-name-directory file)))
      (error "Buffer is not attached to any file"))))

;; mu4e
(add-to-list 'load-path "/usr/local/Cellar/mu/0.9.9.6/share/emacs/site-lisp/mu4e")
(use-package mu4e
  :config
  (progn
    (setq mu4e-get-mail-command "offlineimap"
	  mu4e-update-interval 300
	  mu4e-maildir (expand-file-name "~/mail/personal")
	  mu4e-sent-folder "/Sent"
	  mu4e-drafts-folder "/Drafts"
	  mu4e-trash-folder "/Trash"
	  mu4e-refile-folder "/Archive")))

;; org-journal
(use-package org-journal
  :init (setq org-journal-dir "~/Documents/personal/journal/"))

;; org agenda
(setq org-agenda-files '("~/.org"))

;;; provide OS X package
(provide 'osx)

;;; osx.el ends here
