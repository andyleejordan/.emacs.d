;;; windows.el --- My Windows specific customizations

;;; Commentary:

;; This is part of Andy Jordan's Emacs customizations. See
;; `init.el' for license etc.

;;; Code:

;; Uses Consolas font
(set-face-attribute 'default nil :family "Consolas" :height 100)

;; Windows performance tweaks
(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))

(provide 'windows)

;;; windows.el ends here
