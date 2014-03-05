;;; osx --- OS X specific configurations

;;; Commentary:
;;; Uses Source Code Pro font
;;; http://sourceforge.net/projects/sourcecodepro.adobe/

;;; Code:

;;; sage
(add-to-list 'load-path
	     "/Applications/Sage.app/Contents/Resources/sage/data/emacs")
(ignore-errors
  (require 'sage "sage")
  (setq sage-command "~/bin/sage"))

;; delete by moving to trash
(setq delete-by-moving-to-trash t)

;; Set font
(set-frame-font
 "-apple-Source_Code_Pro-medium-normal-normal-*-*-*-*-*-m-0-iso10646-1" nil t)

;; open file's location in Finder
(defun finder ()
  "Opens file directory in Finder."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (shell-command
         (format "%s %s" (executable-find "open") (file-name-directory file)))
      (error "Buffer is not attached to any file"))))

;;; osx.el ends here
