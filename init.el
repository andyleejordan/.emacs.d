;;; init.el --- Andrew Schwartzmeyer's Emacs customizations  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2019 Andrew Schwartzmeyer

;; Author: Andrew Schwartzmeyer <andrew@schwartzmeyer.com>
;; Created: 30 Aug 2013
;; Homepage: https://github.com/andschwa/.emacs.d

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is my `init.el', there are many like it, but this is my own.
;; It is constantly evolving, and attempts to make use of the best
;; packages and best practices available. GNU Emacs is my favorite
;; piece of software: I would not be the programmer I am today without
;; GNU Emacs. Please take as much or as little from it as you need.

;;; Code:

;; These should be set as early as possible.
(customize-set-variable 'load-prefer-newer t)

(with-eval-after-load 'gnutls
  (custom-set-variables
   '(gnutls-verify-error t)
   '(gnutls-min-prime-bits 3072)))

;;; Package System:
(eval-when-compile
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(customize-set-variable 'straight-cache-autoloads t)
(customize-set-variable 'straight-use-package-by-default t)
(customize-set-variable 'use-package-enable-imenu-support t)
(customize-set-variable 'straight-check-for-modifications '(check-on-save find-when-checking))
(add-to-list 'straight-check-for-modifications 'check-on-save)

(eval-when-compile
  (straight-use-package 'use-package)
  (require 'use-package))

(use-package blackout
  :straight (:host github :repo "raxod502/blackout"))
(use-package bind-key)
(use-package dash)
(use-package f)

;; Save data files consistently.
(use-package no-littering)

(customize-set-variable
 'custom-file (no-littering-expand-etc-file-name "custom.el"))

;;; Helper Functions:
(defmacro add-args-to-list (list-var elements &optional append compare-fn)
  "Adapts `add-to-list' to add multiple ELEMENTS to LIST-VAR.
Pass APPEND and COMPARE-FN to each invocation of `add-to-list'."
  `(dolist (element ,elements) (add-to-list ,list-var element ,append ,compare-fn)))

(defun call-if-fbound (function &rest args)
  "Call FUNCTION with optional ARGS, only if it is `fbound'."
  "Return t if it is fbound and called without error, and nil otherwise."
  (when (fboundp function) (apply function args) t))

;; Colorize strings:
(defmacro with-face (str &rest properties)
  "Return STR with the given face PROPERTIES, suitable for `concat'."
  `(propertize ,str 'face (list ,@properties)))

(defmacro use-feature (name &rest args)
  "Like `use-package' for NAME and ARGS, but with `:straight' nil."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

;;; Platform:
(use-feature local
  :load-path "etc"
  :if (file-readable-p (no-littering-expand-etc-file-name "local.el")))

(use-feature linux
  :load-path "etc"
  :if (eq system-type 'gnu/linux))

(use-feature osx
  :load-path "etc"
  :if (eq system-type 'darwin))

(use-feature windows
  :load-path "etc"
  :if (eq system-type 'windows-nt))

;; Experimental:
(use-feature compile-commands :load-path "etc")

(use-feature edl-mode :load-path "etc")

;;; Cursor and Mark Movement:
(bind-key "C-M-<backspace>" #'delete-pair)
(bind-key "C-M-`" #'raise-sexp)
(bind-key "M-o" #'other-window)
(defun undo-other-window ()
  "The opposite of `other-window'."
  (interactive) (other-window -1))
(bind-key "M-O" #'undo-other-window)
(bind-key [remap delete-char] #'delete-forward-char)

(use-feature subword
  :config (global-subword-mode))

;;; Windows / Frames and the buffers in them
(use-package buffer-move)

(use-package transpose-frame
  :config (bind-key "C-x 4 t" #'transpose-frame))

(use-package windmove ; `S-<left,right,up,down>' to move windows
  :config (windmove-default-keybindings))

(use-feature window
  :disabled
  :no-require
  :custom
  (display-buffer-alist
   '((".*"
      (display-buffer-reuse-window display-buffer-same-window)
      (reusable-frames . t))))
  (even-window-sizes t))

(use-feature winner ; `C-c <left,right>' to undo/redo windows
  :config (winner-mode))

;;; Minibuffer Interface:
(use-feature eldoc
  :blackout)

;; From https://with-emacs.com/posts/tips/quit-current-context/
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
            "Quit is ignored during macro defintion, use \\[kmacro-end-macro] if you want to stop macro definition."))
          (cancel-kbd-macro-events))
        ((active-minibuffer-window)
         (when (get-buffer-window "*Completions*")
           ;; Hide completions first so point stays in active window when
           ;; outside the minibuffer.
           (minibuffer-hide-completions))
         (abort-recursive-edit))
        (t
         ;; If we got this far just use the default so we don't miss
         ;; any upstream changes.
         (keyboard-quit))))

(bind-key* [remap keyboard-quit] #'keyboard-quit-context+)

(use-package prescient
  :config (prescient-persist-mode))

(use-package selectrum
  :config
  (bind-key "C-c M-x" #'selectrum-repeat)
  (selectrum-mode)
  :custom-face
  (selectrum-current-candidate ; Solarized Green
   ((t (:inherit highlight :weight bold :foreground "#859900" ))))
  (selectrum-primary-highlight ; Solarized Yellow
   ((t (:weight bold :foreground "#b58900"))))
  (selectrum-secondary-highlight ; Solarized Magenta
   ((t (:weight bold :foreground "#d33682")))))

(use-package selectrum-prescient
  :config (selectrum-prescient-mode))

(use-feature mb-depth :disabled
  :custom (enable-recursive-minibuffers t)
  :config (minibuffer-depth-indicate-mode))

(use-feature minibuf-eldef :disabled
  :custom (minibuffer-eldef-shorten-default t)
  :config (minibuffer-electric-default-mode))

(use-feature savehist
  :config (savehist-mode))

(use-feature which-func
  :custom (which-func-unknown "")
  :config (which-function-mode))

(use-package which-key
  :blackout
  :config (which-key-mode))

;;; Version Control:
(use-package magit
  :defines magit-file-mode-map magit-dwim-selection
  :config
  ;; C-x M-g . `magit-dispatch'
  (bind-key "C-x g" #'magit-status)
  (bind-key "C-c g" #'magit-file-dispatch magit-file-mode-map)
  (add-to-list 'magit-dwim-selection '(magit-branch-and-checkout nil t))
  :custom
  (magit-save-repository-buffers 'dontask)
  (magit-published-branches nil "Disable confirmation.")
  (magit-diff-refine-hunk 'all "Word diffs."))

(use-package git-commit
  :demand
  :hook (git-commit-mode . (lambda () (set-fill-column 72)))
  :custom (git-commit-major-mode 'markdown-mode)
  :config (global-git-commit-mode))

(use-feature vc-hooks
  :custom
  (vc-ignore-dir-regexp
   (format "\\(%s\\)\\|\\(%s\\)"
           vc-ignore-dir-regexp
           tramp-file-name-regexp)))

;;; Buffers:
(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer))

(use-feature uniquify
  :custom (uniquify-buffer-name-style 'forward))

;;; File Navigation:
(defun find-dot-emacs ()
  "Open `~/.emacs.d/init.el'."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(bind-key "C-c i" #'find-dot-emacs)

(use-feature dired
  :config (bind-key [remap list-directory] #'dired)
  :custom
  (dired-dwim-target t "Enable side-by-side `dired' buffer targets.")
  (dired-recursive-copies 'always "Better recursion in `dired'.")
  (dired-recursive-deletes 'top)
  (dired-listing-switches "-alhv" "Must not contain `-p'."))

(use-package dired-git-info
  :config
  (bind-key ")" #'dired-git-info-mode dired-mode-map)
  (add-hook 'dired-after-readin-hook 'dired-git-info-auto-enable))

(use-feature dired-x
  :config
  (bind-key "C-x C-j" #'dired-jump)
  (bind-key "C-x 4 C-j" #'dired-jump-other-window))

(use-feature project
  :defines project-find-functions
  :config
  (bind-key "C-c f" #'project-find-file)
  (bind-key "M-s P" #'project-find-regexp) ; or `rg-project'
  ;; Similar to project-try-vc but works when VC is disabled.
  (defun project-try-magit (dir)
    (let* ((root (magit-toplevel dir)))
      (and root (cons 'vc root))))
  (add-to-list 'project-find-functions #'project-try-magit t))

(use-feature recentf
  :custom
  (recentf-max-saved-items 200)
  (recentf-auto-cleanup 'never "Disabled for performance with Tramp.")
  :config
  (add-to-list 'recentf-exclude
               (lambda (f) (not (string= (file-truename f) f))))
  ;; Save every five minutes, because Emacs crashes.
  (run-at-time t (* 5 60) #'recentf-save-list)
  (recentf-mode))

(defun selectrum-recentf ()
  "Use `selectrum' to open a recent file."
  (interactive)
  (let ((files (mapcar 'abbreviate-file-name recentf-list)))
    (find-file (selectrum-completing-read "Find recent file: " files nil t))))

(bind-key "C-x C-r" #'selectrum-recentf)

;;; Searching:
(bind-key "M-s g" #'vc-git-grep)

(use-feature isearch
  ;; TODO: Set `search-whitespace-regexp' for fuzzier searching.
  :custom (isearch-allow-scroll t))

(use-package grep
  :config (bind-key "M-s R" #'rgrep)) ; or `rg'

(use-package rg ; `ripgrep'
  :config
  ;; Also see `vc-git-grep' and `project-find-regexp'.
  (bind-key "M-s r" #'rg) ; or `rgrep'
  (bind-key "M-s p" #'rg-project) ; or `project-find-regexp'
  (bind-key "M-s s" #'rg-ask-dwim)
  (rg-define-search rg-ask-dwim
    :query ask :format regexp
    :files "everything" :dir project))

;; Use the same binding as `occur-edit-mode' for other writable modes.
;; Can always initiate with `e' and exit with `C-c C-c' or `C-x C-s'.
;;
;; I have no idea why these are otherwise scattered across `e', `C-x
;; C-q', and `C-c C-p'.

(use-feature replace
  :config
  (bind-key "C-x C-s" #'occur-cease-edit occur-edit-mode-map)
  (add-hook 'occur-mode-hook #'next-error-follow-minor-mode))

(use-feature wdired
  :config (bind-key "e" #'dired-toggle-read-only dired-mode-map)
  :custom (wdired-allow-to-change-permissions t))

(use-package wgrep ; makes `rg' buffers writable too
  :defines grep-mode-map ; kinda
  :config (bind-key "e" #'wgrep-change-to-wgrep-mode grep-mode-map)
  :custom (wgrep-auto-save-buffer t))

;;; Formatting / Indentation / Whitespace:
(customize-set-variable 'indent-tabs-mode nil)
(customize-set-variable 'sentence-end-double-space nil)

(bind-key "C-x \\" #'align-regexp)

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package clang-format
  :after cc-mode
  :defines c-mode-base-map
  :config (bind-key [remap indent-region] #'clang-format-region c-mode-base-map))

(use-package dtrt-indent
  :blackout
  :defines dtrt-indent-hook-mapping-list
  :custom
  (dtrt-indent-verbosity 0)
  (dtrt-indent-min-quality 60)
  :config
  (add-args-to-list 'dtrt-indent-hook-mapping-list
                    '((powershell-mode c/c++/java powershell-indent)
                      (groovy-mode default groovy-indent-offset)))
  (dtrt-indent-global-mode))

(use-package editorconfig :disabled
  :blackout
  :config (editorconfig-mode))

(use-feature indent
  :no-require
  :custom (tab-always-indent 'complete))

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Comment-Commands.html
(use-feature newcomment
  :custom (comment-fill-column 0))

(use-package whitespace-cleanup-mode
  :blackout
  :config (global-whitespace-cleanup-mode))

;;; Editing:
(customize-set-variable 'truncate-lines t)
(bind-key "C-x w" #'toggle-truncate-lines)

(use-feature autorevert
  :blackout
  :custom
  (auto-revert-remote-files t)
  (global-auto-revert-non-file-buffers t)
  :config (global-auto-revert-mode))

(use-feature delsel
  :config (delete-selection-mode))

(use-feature elec-pair
  :config (electric-pair-mode))

(use-package saveplace
  :config
  (or (call-if-fbound #'save-place-mode)
      (call-if-fbound #'save-place)))

(use-package undo-tree
  :blackout
  :defines undo-tree-map
  :custom (undo-tree-enable-undo-in-region nil "This is buggy.")
  :config (global-undo-tree-mode))

(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))

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

(use-feature autoinsert
  :config
  (auto-insert-mode)
  (define-auto-insert
    '(sh-mode . "Bash skeleton")
    [(lambda () (sh-set-shell "bash" t nil))
     '(()
       "#!/bin/bash" \n
       \n
       "set -o errexit" \n
       "set -o pipefail" "\n\n")]))

;;; Tab Completion:
(use-feature hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand)
  :custom (hippie-expand-try-functions-list
           '(try-expand-all-abbrevs
             try-expand-dabbrev-visible
             try-expand-dabbrev ; this buffer
             try-expand-dabbrev-all-buffers
             try-expand-dabbrev-from-kill
             try-expand-whole-kill
             try-complete-file-name-partially
             try-complete-file-name)))

(use-package company
  :blackout
  :config
  (bind-keys
   :map company-active-map
   ;; Tab to complete selection.
   ([tab] . company-complete-selection)
   ("TAB" . company-complete-selection)
   ;; Don't override `isearch'.
   ("C-s" . nil) ("C-M-s" . nil)
   ;; Return only if scrolled.
   :filter (company-explicit-action-p)
   ([return] . company-complete-selection)
   ("RET" . company-complete-selection))
  (global-company-mode)
  :custom
  ;; Smaller list.
  (company-tooltip-limit 7)
  ;; Align signatures to the right.
  (company-tooltip-align-annotations t)
  ;; Never display inline (since we use `eldoc').
  (company-frontends '(company-pseudo-tooltip-frontend))
  ;; Disallow non-matching input if we scrolled.
  (company-require-match #'company-explicit-action-p)
  ;; Search buffers with the same major mode.
  (company-dabbrev-other-buffers t)
  ;; Give backends more time.
  (company-async-timeout 5))

(use-package company-prescient
  :config (company-prescient-mode))

;;; Syntax Checking:
;; Treat backquotes as pairs in text mode.
(use-feature text-mode
  :config
  (add-hook 'text-mode-hook #'turn-on-visual-line-mode)
  (modify-syntax-entry ?\` "$`" text-mode-syntax-table))

(use-feature flymake
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error))
  :config
  ;; Magic from https://stackoverflow.com/a/53858408/1028665
  (defun flymake--transform-mode-line-format (ret)
    "Change the output of `flymake--mode-line-format'."
    (setf (seq-elt (car ret) 1) " Fly") ret)
  (advice-add #'flymake--mode-line-format
              :filter-return #'flymake--transform-mode-line-format))

(use-package flymake-shellcheck
  :hook (sh-mode . flymake-shellcheck-load))

;;; Tags:
(use-package dumb-jump
  :bind
  ("C-c M-." . dumb-jump-go)
  ("C-c M-," . dumb-jump-back))

;; Alternatives include: eglot, irony, cquery, rtags, ggtags, and ycmd.
(use-package lsp-mode
  :hook
  (c-mode-common . lsp-deferred) ; apt-get install clangd-9
  ;; https://jedi.readthedocs.io/en/stable/docs/usage.html#type-hinting
  (python-mode . lsp-deferred) ; pip3 install python-language-server
  :commands lsp
  :custom (lsp-enable-snippet nil))

(use-package company-lsp
  :custom (company-lsp-cache-candidates 'auto))

;;; Spelling:
(use-package flyspell
  ;; Disable on Windows because `aspell' 0.6+ isn't available.
  :unless (eq system-type 'windows-nt)
  :blackout
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  :custom
  (flyspell-mode-map (make-sparse-keymap) "Disable all flyspell bindings")
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra")))

(use-package flyspell-correct
  :after flyspell
  :config (bind-key [remap ispell-word] #'flyspell-correct-wrapper flyspell-mode-map))

(use-package auto-correct
  :blackout
  :hook (flyspell-mode . auto-correct-mode)
  :custom (flyspell-use-global-abbrev-table-p t))

;;; Tools:
(use-package auto-sudoedit
  :blackout
  :commands auto-sudoedit-sudoedit
  :init (defalias 'sudoedit #'auto-sudoedit-sudoedit))

(use-feature compile
  :config (bind-key "C-c c" #'compile)
  :custom
  (compilation-ask-about-save nil)
  (compilation-scroll-output t)
  (compilation-always-kill t)
  (compilation-error-regexp-alist
   (delete 'maven compilation-error-regexp-alist)))

(use-package default-text-scale)

(use-package demangle-mode)

(use-feature ediff
  :custom
  (ediff-diff-options "-w")
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-feature eshell
  :bind ("C-c e" . eshell)
  :custom
  (eshell-visual-commands '("bash" "htop" "fish"))
  (eshell-highlight-prompt nil)
  (eshell-prompt-function
   (lambda ()
     (let ((red       "#dc322f")
           (magenta   "#d33682")
           (blue      "#268bd2")
           (cyan      "#2aa198")
           (green     "#859900")
           (base      "#839496"))
       (concat
        (let ((status eshell-last-command-status))
          (when (not (= status 0))
            (with-face (concat (number-to-string status) " ") :foreground magenta)))
        (with-face "@" :foreground (if (= (user-uid) 0) red blue))
        (with-face (system-name) :foreground base) " "
        ;; TODO: Display more Git info.
        (let ((head (shell-command-to-string "git describe --contains --all HEAD")))
          (unless (string-match "fatal:" head)
            (concat "(" (with-face (replace-regexp-in-string "\n\\'" "" head) :foreground green) ") ")))
        (with-face (replace-regexp-in-string (concat "\\`" (getenv "HOME")) "~" (eshell/pwd))
                   :foreground blue) " "
        (with-face "$" :foreground cyan) " ")))))

(use-feature gud
  :no-require
  :custom (gdb-many-windows t))

(use-package ielm
  :custom (ielm-prompt "> "))

(use-package org
  :straight org-plus-contrib
  :config
  (add-hook 'org-mode-hook #'turn-on-auto-fill)
  (require 'org-tempo) ; Bring back `<s [TAB]'.
  (add-args-to-list 'org-structure-template-alist
                    '(("el" . "src emacs-lisp")
                      ("sh" . "src sh")))
  :custom
  (org-startup-indented nil)
  (org-src-tab-acts-natively t)
  (org-adapt-indentation nil)
  (org-latex-listings t)
  (org-pretty-entities t)
  (org-latex-custom-lang-environments '((C "lstlisting")))
  (org-entities-user '(("join" "\\Join" nil "&#9285;" "" "" "⋈")
                       ("reals" "\\mathbb{R}" t "&#8477;" "" "" "ℝ")
                       ("ints" "\\mathbb{Z}" t "&#8484;" "" "" "ℤ")
                       ("complex" "\\mathbb{C}" t "&#2102;" "" "" "ℂ")
                       ("models" "\\models" nil "&#8872;" "" "" "⊧")))
  (org-export-backends '(html beamer ascii latex md))
  (org-babel-load-languages '((emacs-lisp . t)
                              (shell . t))))

(use-package rainbow-mode) ; highlight color codes like "#aabbcc"

(use-feature re-builder
  :custom (reb-re-syntax 'string))

(use-package restart-emacs
  :bind ("C-c Q" . restart-emacs))

(use-feature woman
  :bind ("C-c m" . woman))

;;; Appearance:
;; TODO: Add `helpful' package

;; Try preferred fonts
(--map-first (member it (font-family-list))
             (set-face-attribute 'default nil :family it :height 120)
             '("Cascadia Code" "Source Code Pro" "Menlo" "Ubuntu Mono"))

(when (display-graphic-p)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))

;; Fix invisible buffer content when X is tunneled
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25474
(when (getenv "DISPLAY")
  (add-to-list 'default-frame-alist '(inhibit-double-buffering . t)))

(use-feature frame
  :custom (blink-cursor-blinks 0))

(use-package fortune-cookie
  :custom
  (fortune-cookie-fortune-string
   "History repeats itself:\nthe first time as tragedy,\nthe second time as farce.")
  (fortune-cookie-cowsay-enable (executable-find "cowsay"))
  (fortune-cookie-cowsay-args '("-f" "tux"))
  :config (fortune-cookie-mode))

(use-package hl-todo
  :defines hl-todo-keyword-faces
  :config
  (add-to-list 'hl-todo-keyword-faces '("ANDY" . "#d0bf8f"))
  (global-hl-todo-mode))

(use-feature paren
  :custom
  (show-paren-delay 0)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config (show-paren-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package solarized-theme
  :if (display-graphic-p)
  :straight (solarized-theme :flavor melpa :host github :repo "bbatsov/solarized-emacs"
                             :fork (:host github :repo "andschwa/solarized-emacs" :branch "completions-faces"))
  :custom
  (solarized-use-variable-pitch nil)
  (x-underline-at-descent-line t)
  :config
  (defun toggle-theme ()
    "Switch between Solarized variants."
    (interactive)
    (cond
     ((member 'solarized-dark custom-enabled-themes)
      (disable-theme 'solarized-dark)
      (load-theme 'solarized-light t))
     ((member 'solarized-light custom-enabled-themes)
      (disable-theme 'solarized-light)
      (load-theme 'solarized-dark t))))
  (load-theme 'solarized-dark t))

(unless (display-graphic-p)
  (load-theme 'tango-dark t))

;; This must be loaded after themes.
(use-package smart-mode-line
  :custom
  ;; Better than `automatic' which is very plain.
  (sml/theme 'respectful)
  (sml/no-confirm-load-theme t)
  (sml/name-width 32)
  (sml/shorten-modes nil)
  (sml/replacer-regexp-list nil)
  :config (sml/setup))

;;; Internal Emacs Configuration:
(customize-set-variable 'gc-cons-threshold (* 100 1024 1024))
(customize-set-variable 'read-process-output-max (* 1024 1024))

;; Fix annoyances.
(defalias 'yes-or-no-p 'y-or-n-p)
(customize-set-variable 'minibuffer-message-timeout 0.5)
(customize-set-variable 'set-mark-command-repeat-pop t)
(customize-set-variable 'delete-by-moving-to-trash t)
(customize-set-variable 'create-lockfiles nil)
(customize-set-variable 'ring-bell-function 'ignore)
(customize-set-variable 'visible-bell t)
(customize-set-variable 'inhibit-startup-screen t)
(set-variable 'disabled-command-function nil)

(use-feature help
  :config
  (bind-key "C-h L" #'find-library)
  (add-hook 'help-mode-hook #'turn-on-visual-line-mode)
  :custom (help-window-select t))

;; Simple is Emacs's built-in miscellaneous package.
(use-feature simple
  :config
  (bind-keys
   ([remap just-one-space] . cycle-spacing)
   ([remap upcase-word] . upcase-dwim)
   ([remap downcase-word] . downcase-dwim)
   ([remap capitalize-word] . capitalize-dwim)
   ([remap zap-to-char] . zap-up-to-char))
  (column-number-mode)
  :custom
  ;; TODO: Maybe set `suggest-key-bindings' to `nil'.
  (save-interprogram-paste-before-kill t)
  (kill-do-not-save-duplicates t)
  (kill-whole-line t)
  (shift-select-mode nil)
  (visual-line-fringe-indicators '(nil right-curly-arrow)))

(use-package super-save
  :blackout
  :defines super-save-triggers
  :custom (super-save-remote-files nil)
  :config
  (add-args-to-list 'super-save-triggers
                    '(dired-jump
                      magit-dispatch
                      magit-file-dispatch
                      magit-refresh
                      magit-status))
  (super-save-mode))

(use-feature files
  :custom
  (find-file-visit-truename t)
  (confirm-kill-emacs 'y-or-n-p)
  (confirm-nonexistent-file-or-buffer t)
  (save-abbrevs 'silently)
  (require-final-newline t)
  (backup-by-copying t)
  (delete-old-versions t)
  (version-control t)
  (auto-save-default nil)
  (large-file-warning-threshold (* 20 1000 1000) "20 megabytes.")
  :config
  (add-to-list
   'backup-directory-alist
   `(,tramp-file-name-regexp . ,(no-littering-expand-var-file-name "tramp/backup/"))))

;;; Language Modes:
(add-args-to-list
 'auto-mode-alist '(("\\.ino\\'"  . c-mode)
                    ("\\.vcsh\\'" . conf-mode)
                    ("\\.zsh\\'"  . sh-mode)))

(use-package apt-sources-list)

(use-package bazel-mode :disabled)

;; OCaml
(use-package tuareg
  :defines tuareg-mode-map
  :bind (:map tuareg-mode-map ([remap indent-region] . ocamlformat)))

(use-package ocamlformat
  :straight (:host github :repo "ocaml-ppx/ocamlformat" :files ("emacs/ocamlformat.el"))
  ;; TODO: May want to limit this to certain files.
  :hook (tuareg-mode . (lambda ()
                         (add-hook 'before-save-hook #'ocamlformat-before-save nil 't)))
  :custom (ocamlformat-show-errors nil))

(use-package dune :disabled)

(use-package utop :disabled) ; OCaml shell

(use-package merlin
  :defines merlin-mode-map
  :hook (tuareg-mode . merlin-mode)
  :bind (:map merlin-mode-map
              ;; TODO: Maybe map phrases to paragraphs.
              ([remap xref-find-definitions] . merlin-locate)
              ([remap xref-pop-marker-stack] . merlin-pop-stack)))

(use-package merlin-eldoc
  :hook (tuareg-mode . merlin-eldoc-setup))
;; OCaml setup ends here

(use-package cmake-mode
  :defines cmake-mode-map
  :bind (:map cmake-mode-map
              ([remap xref-find-definitions] . cmake-help-command)))

(use-package csharp-mode :disabled)

(use-package dockerfile-mode)

(use-package fish-mode :disabled)

(use-package gitattributes-mode)

(use-package gitconfig-mode)

(use-package gitignore-mode)

(use-package groovy-mode :disabled)

(use-package markdown-mode
  :hook
  (markdown-mode . turn-on-auto-fill)
  (markdown-mode . (lambda () (set-fill-column 80)))
  :custom (markdown-command "multimarkdown"))

;; Enables `markdown-edit-code-block'.
(use-package edit-indirect)

(use-package nginx-mode)

(use-package powershell)

(use-package protobuf-mode :disabled)

(use-package puppet-mode :disabled)

(use-package pyvenv
  :hook
  (python-mode . pyvenv-mode)
  (python-mode . pyvenv-tracking-mode))

(use-package blacken
  :blackout
  :hook (python-mode . blacken-mode)
  :custom (blacken-only-if-project-is-blackened t))

(use-package ruby-mode)

(use-package rust-mode
  :custom (rust-format-on-save t))

(use-package systemd)

(use-package ssh-config-mode)

(use-package toml-mode)

(use-package yaml-mode)

;;; Finish Loading:
(server-start)
(provide 'init)

;;; init.el ends here
