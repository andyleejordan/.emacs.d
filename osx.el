;;; osx --- OS X specific configurations

;;; Commentary:
;;; Uses Source Code Pro font
;;; http://sourceforge.net/projects/sourcecodepro.adobe/

;;; Code:

;; pull in environment
(setenv "SHELL" "/usr/local/bin/bash")
(exec-path-from-shell-copy-envs '("GPG_AGENT_INFO"))
(exec-path-from-shell-initialize)

;; key bindings
(bind-key "C-c d" 'dash-at-point)

;; disable toolbar and scrollbar
(tool-bar-mode 0)
(scroll-bar-mode 0)

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

;;; provide OS X package
(provide 'osx)

;;; osx.el ends here
