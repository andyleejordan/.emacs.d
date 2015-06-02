;;; windows --- Windows specific configurations

;;; Commentary:
;;; Uses Consolas font

;;; Code:

;; set font
(set-frame-font
 "-outline-Consolas-normal-normal-normal-mono-16-*-*-*-c-*-iso8859-1" nil t)

;; font size
(set-face-attribute 'default nil :height 120)

(provide 'windows)
;;; windows.el ends here
