;;; andy.el --- Experimental and personal customization. -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Andrew Schwartzmeyer

;; Author: Andrew Schwartzmeyer <andrew@schwartzmeyer.com>
;; Created: 24 May 2020
;; Homepage: https://github.com/andschwa/.emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This code is original unless noted otherwise.
;;
;; It is kind of like my own version of crux:
;; https://github.com/bbatsov/crux
;;
;; Customization and keybindings do not belong here.

;;; Code:

(defgroup andy nil
  "Experimental and personal customization."
  :group 'convenience)

;;; Macros:

(defmacro add-args-to-list (list-var elements &optional append compare-fn)
  "Adapts `add-to-list' to add multiple ELEMENTS to LIST-VAR.
Pass APPEND and COMPARE-FN to each invocation of `add-to-list'."
  `(dolist (element ,elements) (add-to-list ,list-var element ,append ,compare-fn)))

;; Source: https://www.emacswiki.org/emacs/EshellPrompt
(defmacro with-face (str &rest properties)
  "Return STR with the given face PROPERTIES, suitable for `concat'."
  `(propertize ,str 'face (list ,@properties)))

;;; Helpers:

;; Source: https://www.emacswiki.org/emacs/CustomizingBoth
(defun call-if-fbound (function &rest args)
  "Call FUNCTION with optional ARGS, only if it is `fbound'.
Return t if it is fbound and called without error, and nil
otherwise."
  (when (fboundp function) (apply function args) t))

;;; My `hideshow' extension:

(defvar-local hs-hid-all-p nil
  "Local variable tracking the usage of `hs-hide-all' and `hs-show-all'.")

(advice-add #'hs-hide-all :before (lambda () (setq hs-hid-all-p t)))

(advice-add #'hs-show-all :before (lambda () (setq hs-hid-all-p nil)))

(defun hs-toggle-hiding+ (&optional global)
  "Like `hs-toggle-hiding' but can also toggle all blocks.
When at the beginning of the buffer or called with the universal
argument, calls `hs-hide-all' or `hs-show-all' as determined by
the value of `hs-hid-all-p', a buffer local variable set by
advising these functions."
  (interactive "P")
  (if (or global (bobp))
      (if hs-hid-all-p (hs-show-all) (hs-hide-all))
    (hs-toggle-hiding)))

;;; Better `C-g' behavior:

;; Source: https://with-emacs.com/posts/tips/quit-current-context/
(defun keyboard-quit-context+ ()
  "Quit current context.
This function is a combination of `keyboard-quit' and
`keyboard-escape-quit' with some parts omitted and some custom
behavior added."
  (interactive)
  (cond ((region-active-p)
         ;; Avoid adding the region to the window selection.
         (setq saved-region-selection nil)
         (let (select-active-regions)
           (deactivate-mark)))
        ((eq last-command 'mode-exited) nil)
        (current-prefix-arg
         nil)
        (defining-kbd-macro
          (message
           (substitute-command-keys
            "Quit is ignored during macro defintion, use
            \\[kmacro-end-macro] if you want to stop macro
            definition."))
          (cancel-kbd-macro-events))
        ((active-minibuffer-window)
         (when (get-buffer-window "*Completions*")
           ;; Hide completions first so point stays in active window when
           ;; outside the minibuffer.
           (minibuffer-hide-completions))
         (setq this-command 'abort-recursive-edit)
         (abort-recursive-edit))
        (t
         ;; If we got this far just use the default so we don't miss
         ;; any upstream changes.
         (setq this-command 'keyboard-quit)
         (keyboard-quit))))

;;; Shortcuts:

(defun find-dot-emacs ()
  "Open `~/.emacs.d/init.el'."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

;;; Completing functions:

(defun recentf-open-files+ ()
  "Use `completing-read' to open a recent file."
  (interactive)
  (let ((files (mapcar 'abbreviate-file-name recentf-list)))
    (find-file (icomplete-vertical-do nil
                 (completing-read "Find recent file: " files nil t)))))

(defun yank-pop+ (&optional arg)
  "Call `yank-pop' with ARG when appropriate, or offer completion."
  (interactive "*P")
  (if arg (yank-pop arg)
    (let* ((old-last-command last-command)
           (enable-recursive-minibuffers t)
           (text (icomplete-vertical-do (:separator 'dotted-line)
                   (completing-read
                    "Yank: "
                    (cl-remove-duplicates
                     kill-ring :test #'string= :from-end t)
                    nil t nil nil)))
           ;; Find `text' in `kill-ring'.
           (pos (cl-position text kill-ring :test #'string=))
           ;; Translate relative to `kill-ring-yank-pointer'.
           (n (+ pos (length kill-ring-yank-pointer))))
      (unless (string= text (current-kill n t))
        (error "Could not setup for `current-kill'"))
      ;; Restore `last-command' over Selectrum commands.
      (setq last-command old-last-command)
      ;; Delegate to `yank-pop' if appropriate or just insert.
      (if (eq last-command 'yank)
          (yank-pop n) (insert-for-yank text)))))

;;; Light and dark theming:
(defcustom light-theme 'solarized-light
  "Light theme to load."
  :group 'andy
  :type 'symbol)

(defcustom dark-theme 'solarized-dark
  "Dark theme to load."
  :group 'andy
  :type 'symbol)

(defun load-light-theme ()
  (interactive)
  (disable-theme dark-theme)
  (load-theme light-theme t))

(defun load-dark-theme ()
  (interactive)
  (disable-theme light-theme)
  (load-theme dark-theme t))

(defun toggle-theme ()
  "Switch between theme variants."
  (interactive)
  (cond
   ((member dark-theme custom-enabled-themes)
    (load-light-theme))
   ((member light-theme custom-enabled-themes)
    (load-dark-theme))))

;; Schedule:
(run-at-time "05:00am" (* 60 60 24) #'load-light-theme)
(run-at-time "05:00pm" (* 60 60 24) #'load-dark-theme)

;;; Skeletons:
;; https://www.gnu.org/software/emacs/manual/html_mono/autotype.html#Skeleton-Language

(define-skeleton c-ifdef-skeleton
  "Wraps code with a C pre-processor conditional block."
  (completing-read "#if defined(IDENTIFIER): " '("__WINDOWS__"))
  "#if defined(" str ")\n"
  > - "\n"
  "#else" \n
  > _ "\n"
  "#endif // defined(" str ")" \n)

(provide 'andy)
;;; andy.el ends here
