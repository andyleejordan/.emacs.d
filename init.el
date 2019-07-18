;;; init.el --- Andrew Schwartzmeyer's Emacs customizations

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
(add-to-list 'straight-check-for-modifications 'check-on-save)

(eval-when-compile
  (straight-use-package 'use-package)
  (require 'use-package))

(use-package delight)
(use-package bind-key)
(use-package dash)
(use-package f)

;; Save data files consistently:
;; - `save-place-file'
;; - `undo-tree-history-directory-alist'
;; - `backup-directory-alist'
;; - etc.
(use-package no-littering)

(customize-set-variable
 'custom-file (no-littering-expand-var-file-name "custom.el"))

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
(use-feature linux
  :load-path "lisp"
  :if (eq system-type 'gnu/linux))

(use-feature osx
  :load-path "lisp"
  :if (eq system-type 'darwin))

(use-feature windows
  :load-path "lisp"
  :if (eq system-type 'windows-nt))

;; Experimental:
(use-feature compile-commands
  :load-path "lisp")

(use-feature edl-mode
  :load-path "lisp")

;;; Cursor and Mark Movement:
(bind-key "M-o" #'other-window)
(bind-key "C-M-<backspace>" #'delete-pair)
(bind-key "C-M-`" #'raise-sexp)

(use-package imenu-anywhere
  :bind (("C-c i" . ivy-imenu-anywhere)))

(use-feature isearch
  :custom (isearch-allow-scroll t))

(use-feature subword
  :config (global-subword-mode))

(use-package swiper
  :defines swiper-map
  :demand
  :bind
  (([remap isearch-forward] . swiper-isearch)
   ([remap isearch-backward] . swiper-isearch-backward)
   ([remap isearch-forward-symbol-at-point] . swiper-isearch-thing-at-point)
   :map search-map ; `M-s'
   ("M-c" . swiper-from-isearch) ; local binding
   ("M-r" . counsel-rg)
   ("M-t" . swiper-isearch-toggle)
   :map swiper-map
   ;; NOTE: `swiper-isearch-toggle' ought to work too but doesn't.
   ("M-t" . swiper-from-isearch))
  :custom-face
  ;; Change from yellow to magenta.
  (swiper-match-face-2 ((t :foreground "#d33682")))
  (swiper-match-face-3 ((t :foreground "#d33682")))
  (swiper-match-face-4 ((t :foreground "#d33682"))))

;;; Windows / Frames and the buffers in them
(use-package buffer-move
  :bind (("C-S-<up>"    . buf-move-up)
         ("C-S-<down>"  . buf-move-down)
         ("C-S-<left>"  . buf-move-left)
         ("C-S-<right>" . buf-move-right)))

(use-package transpose-frame
  :commands (transpose-frame-get-arrangement
             transpose-frame-set-arrangement
             transpose-frame
             flip-frame
             flop-frame
             rotate-frame
             rotate-frame-clockwise
             rotate-frame-anticlockwise)
  :bind (:map ctl-x-4-map ("t" . transpose-frame)))

(use-package windmove ; `S-<left,right,up,down>' to move windows
  :config (windmove-default-keybindings))

(use-feature winner ; `C-c <left,right>' to undo/redo windows
  :config (winner-mode))

;;; Minibuffer Interface:
(bind-key "L" #'find-library help-map)

(defalias 'al   #'align)
(defalias 'alr  #'align-regexp)
(defalias 'cg   #'customize-group)
(defalias 'dc   #'desktop-clear)
(defalias 'dml  #'delete-matching-lines)
(defalias 'dnml #'delete-non-matching-lines)
(defalias 'eb   #'eval-buffer)
(defalias 'er   #'eval-region)
(defalias 'rb   #'revert-buffer)
(defalias 'sl   #'sort-lines)
(defalias 'spa  #'straight-pull-all)
(defalias 'tdoe #'toggle-debug-on-error)
(defalias 'vlm  #'visual-line-mode)
(defalias 'wsc  #'whitespace-cleanup)
(defalias 'wsm  #'whitespace-mode)

(use-package amx
  :custom (amx-history-length history-length))

(use-package counsel
  :delight
  :demand
  :config (counsel-mode)
  :bind
  ;; Note that `counsel-mode' rebinds most commands.
  (([remap bookmark-jump] . counsel-bookmark)
   ([remap find-library]  . counsel-find-library)
   ([remap imenu]         . counsel-imenu)
   ([remap yank-pop]      . counsel-yank-pop)
   ("C-c f"               . counsel-git)
   ("C-c C-SPC"           . counsel-mark-ring)
   ("C-c l"               . counsel-locate)
   ("C-x C-r"             . counsel-recentf))
  :custom
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp "\(?:\‘[#.]\)\|\(?:[#~]\’\)"))

(use-feature eldoc
  :delight)

(use-package hydra)

(use-package ivy
  :delight
  :demand
  :config (ivy-mode)
  :bind (("C-c M-x" . ivy-resume)
         :map ivy-minibuffer-map
         ("C-r" . ivy-previous-line-or-history))
  :custom
  (ivy-height 8)
  (ivy-use-virtual-buffers t "Add recentf to buffers.")
  (ivy-virtual-abbreviate 'abbreviate "And show with path.")
  (ivy-initial-inputs-alist nil "Don't start with '^'.")
  (ivy-display-style 'fancy)
  :custom-face
  ;; Remove underline.
  (ivy-current-match ((t (:underline nil))))
  ;; Italicize variables declared with `defcustom'.
  (ivy-highlight-face ((t (:inherit nil :slant italic)))))

(use-package ivy-hydra)

(use-feature mb-depth
  :custom (enable-recursive-minibuffers t)
  :config (minibuffer-depth-indicate-mode))

(use-package minibuf-eldef
  :custom (minibuffer-eldef-shorten-default t)
  :config (minibuffer-electric-default-mode))

(use-feature savehist
  :config (savehist-mode))

(use-package which-key
  :delight
  :config (which-key-mode))

;;; Version Control:
(use-package magit
  :defines magit-file-mode-map
  :straight (magit :host github :repo "magit/magit" :branch "master")
  :demand
  ;; C-x M-g . `magit-dispatch'
  ;; C-c M-g . `magit-file-dispatch'
  :bind (("C-x g" . magit-status))
  :custom
  ;; TODO: Maybe `(magit-dwim-selection '((magit-branch-and-checkout nil t)))'
  (magit-completing-read-function #'ivy-completing-read)
  (magit-save-repository-buffers 'dontask)
  (magit-published-branches nil "Disable confirmation.")
  (magit-diff-refine-hunk 'all "Word diffs."))

;; TODO: Figure out why the forge package doesn't bring its own
;; dependencies.
(use-package closql)
(use-package ghub)
(use-package forge
  :after markdown-mode)

(use-package git-commit
  :hook (git-commit-mode . (lambda () (set-fill-column 72)))
  :custom (git-commit-major-mode 'markdown-mode)
  :config (global-git-commit-mode))

;;; Buffers:
(use-package ibuffer
  :bind (([remap list-buffers] . ibuffer)))

(use-feature uniquify
  :custom (uniquify-buffer-name-style 'forward))

(use-feature project
  :defines project-find-functions
  :config
  ;; Similar to project-try-vc but works when VC is disabled.
  (defun project-try-magit (dir)
    (let* ((root (magit-toplevel dir)))
      (and root (cons 'vc root))))
  (add-to-list 'project-find-functions #'project-try-magit t))

(use-feature vc-hooks
  :custom
  (vc-ignore-dir-regexp
   (format "\\(%s\\)\\|\\(%s\\)"
           vc-ignore-dir-regexp
           tramp-file-name-regexp)))

;;; File Navigation:
(use-feature dired-loaddefs)

(use-feature dired
  :hook (dired-mode . dired-hide-details-mode)
  :bind ([remap list-directory] . dired)
  :custom
  (dired-dwim-target t "Enable side-by-side `dired' buffer targets.")
  (dired-recursive-copies 'always "Better recursion in `dired'.")
  (dired-recursive-deletes 'top)
  (dired-listing-switches "-alhvp"))

(use-feature dired-x
  :bind (("C-x C-j"   . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window)))

(use-feature recentf
  :custom
  (recentf-max-saved-items 256)
  (recentf-auto-cleanup 'never "Disabled for performance with Tramp.")
  :config (add-to-list 'recentf-exclude
                       (lambda (f) (not (string= (file-truename f) f))))
  (recentf-mode))

(use-package rg ; `ripgrep'
  :bind (:map search-map ; `M-s'
              ("s" . rg-dwim)
              ("r" . rg-project))
  :commands (rg-literal))

;; Use the same binding as `occur-edit-mode' for other writable modes.
;; Can always initiate with `e' and exit with `C-c C-c' or `C-x C-s'.
;;
;; I have no idea why these are otherwise scattered across `e', `C-x
;; C-q', and `C-c C-p'.

(use-feature replace
  :bind (:map occur-edit-mode-map ("C-x C-s" . occur-cease-edit)))

(use-feature wdired
  :bind (:map dired-mode-map ("e" . dired-toggle-read-only))
  :custom (wdired-allow-to-change-permissions t))

(use-package wgrep ; makes `rg' buffers writable too
  :defines grep-mode-map
  :bind (:map grep-mode-map ("e" . wgrep-change-to-wgrep-mode))
  :custom (wgrep-auto-save-buffer t))

;;; Formatting / Indentation / Whitespace:
(customize-set-variable 'indent-tabs-mode nil)
(customize-set-variable 'sentence-end-double-space nil)

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package clang-format
  :defines c-mode-base-map
  :after cc-mode
  :bind (:map c-mode-base-map ([remap indent-region] . clang-format-region)))

(use-package dtrt-indent
  :delight
  :defines dtrt-indent-hook-mapping-list
  :custom
  (dtrt-indent-verbosity 0)
  (dtrt-indent-min-quality 60)
  :config
  (add-args-to-list 'dtrt-indent-hook-mapping-list
                    '((powershell-mode c/c++/java powershell-indent)
                      (groovy-mode default groovy-indent-offset)))
  (dtrt-indent-global-mode))

(use-package editorconfig
  :delight
  :config (editorconfig-mode))

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Comment-Commands.html
(use-feature newcomment
  :custom (comment-fill-column 0))

(use-package whitespace
  :commands (whitespace-mode))

(use-package ws-butler
  :delight
  :custom (ws-butler-keep-whitespace-before-point nil)
  :config (ws-butler-global-mode))

;;; Editing:
(customize-set-variable 'truncate-lines t)

(use-feature autorevert
  :delight auto-revert-mode
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

(use-package unfill
  :bind (([remap fill-paragraph] . unfill-toggle)))

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

;;; Completion / Syntax / Tags:
(customize-set-variable 'tab-always-indent 'complete)

;; Treat backquotes as pairs in text mode.
(modify-syntax-entry ?\` "$`" text-mode-syntax-table)

(use-package dumb-jump
  :bind (("C-c M-." . dumb-jump-go)
         ("C-c M-," . dumb-jump-back))
  :custom
  (dumb-jump-max-find-time 8 "Wait longer for remote systems.")
  (dumb-jump-selector 'ivy))

(use-feature flymake
  :hook ((emacs-lisp-mode sh-mode) . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package flymake-shellcheck
  :hook (sh-mode . flymake-shellcheck-load))

(use-package flycheck-tip) ; also for flymake

(use-feature hippie-exp
  :bind (([remap dabbrev-expand] . hippie-expand))
  :custom (hippie-expand-try-functions-list
           '(try-expand-all-abbrevs
             try-expand-dabbrev-visible
             try-expand-dabbrev ; this buffer
             try-expand-dabbrev-all-buffers
             try-expand-dabbrev-from-kill
             try-expand-whole-kill
             try-complete-file-name-partially
             try-complete-file-name)))

;; options include irony, cquery, rtags, ggtags, and ycmd
(use-package lsp-mode
  ;; automatically sets up flymake
  :commands (lsp)
  :defines clangd-enabled
  :hook (c-mode-common . (lambda ()
                           (or
                            (boundp 'clangd-enabled)
                            (when (setq clangd-enabled
                                        (y-or-n-p "Start clangd? "))
                              (lsp)))))
  :custom (lsp-enable-snippet nil)
  :custom-face
  (lsp-face-highlight-textual ((t (:background unspecified))))
  ;; Solarized Red
  (lsp-face-highlight-read ((t (:background "#DC322F"))))
  ;; Solarized Cyan
  (lsp-face-highlight-write ((t (:background "#2AA198")))))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

;; Use `omnisharp-install-server' to setup.
(use-package omnisharp
  :defines omnisharp-mode-map
  :hook (csharp-mode . omnisharp-mode)
  :custom (omnisharp-imenu-support t)
  :bind (:map omnisharp-mode-map
              ([remap xref-find-definitions] . omnisharp-go-to-definition)
              ([remap xref-find-references] . omnisharp-find-usages)
              ;; `xref-pop-marker-stack' works as expected.
              ([remap indent-region] . omnisharp-code-format-region)))

(use-package smart-tab
  :delight
  :custom (smart-tab-using-hippie-expand t)
  :config (global-smart-tab-mode))

(use-feature xref
  :custom (xref-show-xrefs-function #'ivy-xref-show-xrefs)
  :config (use-package ivy-xref))

;;; Spelling:
(use-package flyspell
  ;; Disable on Windows because `aspell' 0.6+ isn't available.
  :if (not (eq system-type 'windows-nt))
  :delight
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  :custom
  (flyspell-mode-map (make-sparse-keymap) "Disable all flyspell bindings")
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra")))

(use-package flyspell-correct
  :bind (:map flyspell-mode-map
              ([remap ispell-word] . flyspell-correct-wrapper))
  :config (use-package flyspell-correct-ivy))

(use-package auto-correct
  :delight
  :custom (flyspell-use-global-abbrev-table-p t)
  :hook (flyspell-mode . auto-correct-mode))

;;; Tools:
;; TODO: Add `sudo-edit' package
(use-feature compile
  :bind (("C-c c" . compile))
  :custom
  (compilation-ask-about-save nil)
  (compilation-scroll-output t)
  (compilation-always-kill t)
  (compilation-error-regexp-alist
   (delete 'maven compilation-error-regexp-alist)))

(use-package default-text-scale)

(use-package demangle-mode
  :commands (demangle-mode))

(use-feature ediff
  :custom
  (ediff-diff-options "-w")
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package elmacro ; show macros as Emacs Lisp
  :delight
  :config (elmacro-mode))

(use-package eshell
  :commands (eshell)
  :bind (("C-c e" . eshell))
  :custom
  (eshell-visual-commands '("bash" "htop" "fish"))
  (eshell-prompt-regexp "^> ")
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
                   :foreground blue) "\n"
        (with-face ">" :foreground cyan) " ")))))

(use-feature gud
  :custom (gdb-many-windows t))

(use-package ielm
  :commands (ielm)
  :custom (ielm-prompt "> "))

(use-package org
  :straight org-plus-contrib
  :hook (org-mode . turn-on-auto-fill)
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
  (org-export-backends '(html beamer ascii latex md)))

(use-package org-download
  :commands (org-download-yank))

(use-package rainbow-mode ; highlight color codes like "#aabbcc"
  :commands (rainbow-mode))

(use-feature re-builder
  :commands (re-builder regexp-builder)
  :custom (reb-re-syntax 'string))

(use-package restart-emacs
  :bind (("C-c Q" . restart-emacs)))

(use-feature woman
  :bind (("C-c m" . woman)))

;;; Appearance:
;; TODO: Add `helpful' package
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
   "History repeats itself: the first time as tragedy, the second time as farce.")
  (fortune-cookie-cowsay-enable (executable-find "cowsay"))
  (fortune-cookie-cowsay-args '("-f" "tux"))
  :config (fortune-cookie-mode))

(use-package hl-todo
  :config (global-hl-todo-mode))

(use-feature paren
  :custom (show-paren-delay 0)
  :config (show-paren-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smart-mode-line
  :custom
  (sml/no-confirm-load-theme t)
  (sml/theme 'respectful)
  (sml/name-width 32)
  (sml/shorten-modes nil)
  (sml/replacer-regexp-list nil)
  :config (sml/setup))

(use-package solarized-theme
  :if (display-graphic-p)
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

(when (equal (system-name) "andschwa-oe")
  (set-face-attribute 'default nil :family "Ubuntu Mono" :height 160)
  (disable-theme 'solarized-dark)
  (load-theme 'solarized-light t))

(unless (display-graphic-p)
  (load-theme 'tango-dark t))

;;; Internal Emacs Configuration:
(customize-set-variable 'gc-cons-threshold 20000000)

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

;; Save Emacs sessions
(use-feature desktop
  :custom (desktop-restore-frames nil)
  :config (desktop-save-mode))

;; Simple is Emacs's built-in miscellaneous package.
(use-feature simple
  :bind (([remap just-one-space] . cycle-spacing)
         ([remap upcase-word] . upcase-dwim)
         ([remap downcase-word] . downcase-dwim)
         ([remap capitalize-word] . capitalize-dwim)
         ([remap zap-to-char] . zap-up-to-char))
  :custom
  (mark-ring-max 1024)
  (global-mark-ring-max 1024)
  (save-interprogram-paste-before-kill t)
  (kill-do-not-save-duplicates t)
  (kill-whole-line t)
  (shift-select-mode nil)
  (visual-line-fringe-indicators '(nil right-curly-arrow))
  :config
  (column-number-mode)
  (dolist (mode '(compilation-mode-hook text-mode-hook help-mode))
    (add-hook mode 'turn-on-visual-line-mode)))

(use-package super-save
  :delight
  :defines super-save-triggers
  :config
  (add-args-to-list 'super-save-triggers
                    '(magit-refresh
                      magit-status
                      counsel-find-file
                      dired-jump
                      ivy-switch-buffer))
  (super-save-mode))

(use-feature tramp
  :defer
  :custom
  (tramp-verbose 2) ; Only warnings
  (tramp-default-method "ssh")
  (tramp-ssh-controlmaster-options
   (concat
    ;; Force a shared connection.
    "-o ControlMaster=yes "
    "-o ControlPath='tramp.%%C' "
    ;; Keep it open but not indefinitely.
    "-o ControlPersist=4h"))
  ;; Cache file attributes and directories for a minute
  (remote-file-name-inhibit-cache 60)
  (tramp-completion-reread-directory-timeout 60))

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

(use-package bazel-mode)

(use-package caml
  :straight (:host github :repo "ocaml/caml-mode"))

(use-package ocamlformat
  :straight (:host github :repo "ocaml-ppx/ocamlformat" :files ("emacs/ocamlformat.el"))
  :custom (ocamlformat-show-errors nil)
  ;; TODO: May want to limit this to certain files.
  :hook ((tuareg-mode) . (lambda ()
                           (add-hook 'before-save-hook 'ocamlformat-before-save nil 't))))

(use-package tuareg
  :defines tuareg-mode-map
  :bind (:map tuareg-mode-map ([remap indent-region] . ocamlformat)))

(use-package utop) ; OCaml shell

(use-package merlin
  :defines merlin-mode-map
  :bind (:map merlin-mode-map
              ;; TODO: Maybe map phrases to paragraphs.
              ([remap xref-find-definitions] . merlin-locate)
              ([remap xref-pop-marker-stack] . merlin-pop-stack))
  :hook ((tuareg-mode caml-mode) . merlin-mode))

(use-package merlin-eldoc
  :hook ((tuareg-mode caml-mode) . merlin-eldoc-setup))

(use-package ocp-indent)

(use-package cmake-mode
  :defines cmake-mode-map
  :bind (:map cmake-mode-map
              ([remap xref-find-definitions] . cmake-help-command)))

(use-package csharp-mode
  :custom (csharp-want-imenu nil))

(use-package dockerfile-mode)

(use-package fish-mode)

(use-package gitattributes-mode)

(use-package gitconfig-mode)

(use-package gitignore-mode)

(use-package groovy-mode)

(use-package json-mode)

(use-package markdown-mode
  :defines markdown-mode-syntax-table
  :hook
  (markdown-mode . turn-on-auto-fill)
  (markdown-mode . (lambda () (set-fill-column 80)))
  :custom (markdown-command "multimarkdown"))

(use-package edit-indirect)

(use-package nginx-mode)

(use-package powershell)

(use-package protobuf-mode)

(use-package puppet-mode)

(use-package ruby-mode)

(use-package rust-mode
  :custom (rust-format-on-save t))

(use-package ssh-config-mode)

(use-package toml-mode)

(use-package web-mode)

(use-package yaml-mode)

;;; Finish Loading:
(server-start)
(provide 'init)

;;; init.el ends here
