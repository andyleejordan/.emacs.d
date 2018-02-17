;;; windows --- Windows specific configurations

;;; Commentary:
;;; Uses Consolas font

;;; Code:
(set-face-attribute 'default nil :family "Consolas" :height 100)

;; Windows performance tweaks
(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))

(provide 'windows)
;;; windows.el ends here
