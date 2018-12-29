;;; init.el --- Andrew Schwartzmeyer's Emacs customizations

;; Copyright (C) 2013-2018 Andrew Schwartzmeyer

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
  (load bootstrap-file nil 'nomessage))

(customize-set-variable 'straight-cache-autoloads t)
(customize-set-variable 'straight-use-package-by-default t)
(straight-use-package 'use-package)
(eval-when-compile (require 'use-package))

(use-package benchmark-init
  :demand
  :hook (after-init . benchmark-init/deactivate)
  :config (benchmark-init/activate))

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

;;; Cursor and Mark Movement:
(bind-key "M-o" #'other-window)
(bind-key "C-M-<backspace>" #'delete-pair)
(bind-key "C-M-`" #'raise-sexp)

(use-package iedit)

(use-package imenu-anywhere
  :bind (("C-c i" . ivy-imenu-anywhere)))

(use-feature isearch
  :custom (isearch-allow-scroll t))

(use-feature subword
  :config (global-subword-mode))

(use-package swiper
  :bind (:map search-map ("s" . swiper-from-isearch)))

(use-package windmove
  :config (windmove-default-keybindings))

;;; Windows / Frames
(use-package transpose-frame
  :commands (transpose-frame-get-arrangement
             transpose-frame-set-arrangement
             transpose-frame
             flip-frame
             flop-frame
             rotate-frame
             rotate-frame-clockwise
             rotate-frame-anticlockwise)
  :bind (:map ctl-x-5-map ("r" . rotate-frame-anticlockwise)))

(use-feature winner
  ;; Enables undo/redo of windows configurations with
  ;; `C-c <left/right>'.
  :config (winner-mode))

;;; Minibuffer Interface:
(bind-key "L" #'find-library help-map)

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
   ("C-x C-r"             . counsel-recentf)
   :map search-map
   ("M-r"                 . counsel-rg)
   ("g"                   . counsel-grep-or-swiper))
  :custom
  (counsel-grep-base-command
   "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp "\(?:\‘[#.]\)\|\(?:[#~]\’\)"))

(use-feature eldoc
  :delight)

(use-package hydra)

(use-package ivy
  :delight
  :demand
  :config (ivy-mode)
  :bind (("C-c M-x" . ivy-resume))
  :custom
  (ivy-height 8)
  (ivy-use-virtual-buffers t "Add recentf to buffers.")
  (ivy-virtual-abbreviate 'abbreviate "And show with path.")
  (ivy-initial-inputs-alist nil "Don't start with '^'.")
  (ivy-display-style 'fancy)
  :custom-face
  ;; Remove underline.
  (ivy-current-match ((t (:underline nil))))
  ;; Fix match colors.
  (ivy-minibuffer-match-face-1 ((t :foreground "#b58900"))) ; orange for substring
  (ivy-minibuffer-match-face-2 ((t :foreground "#268bd2"))) ; blue for initials
  (ivy-minibuffer-match-face-3 ((t :foreground "#268bd2")))
  (ivy-minibuffer-match-face-4 ((t :foreground "#268bd2")))
  ;; Italicize variables declared with `defcustom'.
  (ivy-highlight-face ((t (:inherit nil :slant italic)))))

(use-package ivy-hydra)

(use-feature mb-depth
  :custom (enable-recursive-minibuffers t)
  :config (minibuffer-depth-indicate-mode))

(use-package minibuf-eldef
  :custom (minibuffer-eldef-shorten-default t)
  :config (minibuffer-electric-default-mode))

(use-package prescient
  :config (prescient-persist-mode))

(use-package ivy-prescient
  :config
  (add-args-to-list 'ivy-prescient-sort-commands
                    '(counsel-find-library
                      counsel-git
                      counsel-imenu
                      counsel-recentf
                      counsel-bookmark))
  (ivy-prescient-mode))

(use-package which-key
  :delight
  :config (which-key-mode))

;;; Version Control:
(use-package magit
  :straight (magit :host github :repo "magit/magit" :branch "maint")
  :demand
  :bind (("C-x g"   . magit-status)
         ("C-x C-g" . magit-dispatch-popup)
         :map magit-file-mode-map
         ("C-c g"   . magit-file-popup))
  :custom
  (magit-completing-read-function #'ivy-completing-read)
  (magit-save-repository-buffers 'dontask)
  (magit-published-branches nil "Disable confirmation.")
  ;; TODO: Maybe `(magit-dwim-selection '((magit-branch-and-checkout nil t)))'
  :config
  (magit-define-popup-switch 'magit-push-popup ?u
    "Set upstream" "--set-upstream"))

(use-package git-commit
  :hook (git-commit-mode . (lambda () (set-fill-column 72)))
  :custom (git-commit-major-mode 'markdown-mode)
  :config (global-git-commit-mode))

(use-package git-gutter
  :disabled
  :delight
  :demand
  :config (global-git-gutter-mode)
  :bind (("C-x v s" . git-gutter:stage-hunk)
         ("C-x v n" . git-gutter:next-hunk)
         ("C-x v p" . git-gutter:previous-hunk)))

(use-feature vc-hooks
  ;; Replaced by Magit. Disabled for performance.
  :custom (vc-handled-backends nil))

;;; Buffers:
(use-package buffer-move
  :bind (("C-S-<up>"    . buf-move-up)
         ("C-S-<down>"  . buf-move-down)
         ("C-S-<left>"  . buf-move-left)
         ("C-S-<right>" . buf-move-right)))

(use-package ibuffer
  :bind (([remap list-buffers] . ibuffer)))

(use-package ibuffer-vc)

(use-feature midnight
  ;; Kill old buffers at midnight.
  :config (midnight-mode))

(use-feature uniquify
  :custom (uniquify-buffer-name-style 'forward))

;;; File Navigation:
(use-feature dired-loaddefs)

(use-feature dired
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (dired-dwim-target t "Enable side-by-side `dired' buffer targets.")
  (dired-recursive-copies 'always "Better recursion in `dired'.")
  (dired-recursive-deletes 'top)
  (dired-listing-switches "-alhvp"))

(use-feature dired-x
  :bind (("C-x C-j"   . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window)))

(use-package find-file-in-project ; `ffip'
  :bind (("C-c C-f" . find-file-in-project-by-selected))
  :custom (ffip-use-rust-fd (executable-find "fd")))

(use-feature recentf
  :custom
  (recentf-max-saved-items 256)
  (recentf-auto-cleanup 'never "Disabled for performance with Tramp.")
  :config (add-args-to-list 'recentf-exclude
                            '(;; exclude files in paths with symlinks
                              ;; (not just files which are symlinks)
                              (lambda (f) (not (string= (file-truename f) f)))))
  (recentf-mode))

(use-package rg ; `ripgrep'
  :bind (:map search-map
              ("r" . rg-project)
              ("d" . rg-dwim))
  :commands (rg-project rg-literal))

(use-feature wdired
  :custom (wdired-allow-to-change-permissions t))

(use-package wgrep ; makes `rg' buffers writable too
  :bind (:map grep-mode-map ("C-x C-q" . wgrep-change-to-wgrep-mode))
  :custom (wgrep-auto-save-buffer t))

;;; Formatting / Indentation / Whitespace:
(customize-set-variable 'indent-tabs-mode nil)
(customize-set-variable 'sentence-end-double-space nil)

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package clang-format
  :after cc-mode
  :bind (:map c-mode-base-map ([remap indent-region] . clang-format-region)))

(use-package dtrt-indent
  :delight
  :custom (dtrt-indent-min-quality 60)
  :config
  (add-args-to-list 'dtrt-indent-hook-mapping-list
                    '((powershell-mode c/c++/java powershell-indent)
                      (groovy-mode default groovy-indent-offset)))
  (dtrt-indent-global-mode))

(use-package editorconfig
  :delight
  :config (editorconfig-mode))

(use-package whitespace
  :commands (whitespace-mode))

(use-package ws-butler
  :delight
  :custom (ws-butler-keep-whitespace-before-point nil)
  :config (ws-butler-global-mode))

;;; Editing:
;; TODO: Maybe add `iedit'
(customize-set-variable 'truncate-lines t)

(use-feature autorevert
  :delight auto-revert-mode
  :custom (auto-revert-verbose nil)
  :config (global-auto-revert-mode))

(use-feature delsel
  :config (delete-selection-mode))

(use-feature elec-pair
  :config (electric-pair-mode))

(use-package saveplace
  :config
  (or (call-if-fbound #'save-place-mode)
      (call-if-fbound #'save-place)))

(use-package whole-line-or-region
  ;; This replaces `kill-whole-line' on <C-S-backspace> with `C-w'
  :delight whole-line-or-region-local-mode
  :config (whole-line-or-region-global-mode))

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

(use-package flymake
  :hook
  (emacs-lisp-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package flymake-shell)

;; also for flymake
(use-package flycheck-tip)

(use-package flycheck
  :bind-keymap (("C-c !" . flycheck-mode-map))
  :config (global-flycheck-mode))

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
  :commands (lsp)
  :custom (lsp-prefer-flymake nil) ; use flycheck instead
  :custom-face
  (lsp-face-highlight-textual ((t (:background unspecified))))
  ;; Solarized Red
  (lsp-face-highlight-read ((t (:background "#DC322F"))))
  ;; Solarized Cyan
  (lsp-face-highlight-write ((t (:background "#2AA198")))))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package cquery
  :hook (c-mode-common . (lambda ()
                           (or
                            (boundp 'cquery-enabled)
                            (when (setq cquery-enabled
                                        (y-or-n-p "Start cquery? "))
                              (lsp)))))
  :custom
  (cquery-executable
   (no-littering-expand-var-file-name "cquery/build/release/bin/cquery"))
  (cquery-extra-init-params '(:completion (:detailedLabel t))))

;; Use `omnisharp-install-server' to setup.
(use-package omnisharp
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
  (compilation-scroll-output 'first-error)
  (compilation-always-kill t))

(use-package demangle-mode
  :commands (demangle-mode))

(use-package elmacro
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

(use-package ielm
  :commands (ielm)
  :custom (ielm-prompt "> "))

(use-package keyfreq
  :config
  (keyfreq-mode)
  (keyfreq-autosave-mode))

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

(use-package rainbow-mode
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
(when (getenv "SSH_CLIENT")
  (add-to-list 'default-frame-alist '(inhibit-double-buffering . t)))

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
  :custom
  (show-paren-delay 0)
  (show-paren-style 'expression)
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

(use-package smooth-scrolling
  :disabled
  :delight
  :custom (smooth-scroll-margin 2)
  :config (smooth-scrolling-mode))

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

(unless (display-graphic-p)
  (load-theme 'tango-dark t))

;;; Internal Emacs Configuration:
(customize-set-variable 'gc-cons-threshold 20000000)

;; Fix annoyances.
(customize-set-variable 'minibuffer-message-timeout 0.5)
(customize-set-variable 'set-mark-command-repeat-pop t)
(customize-set-variable 'delete-by-moving-to-trash t)
(customize-set-variable 'create-lockfiles nil)
(customize-set-variable 'ring-bell-function 'ignore)
(customize-set-variable 'visible-bell t)
(customize-set-variable 'inhibit-startup-screen t)
(set-variable 'disabled-command-function nil)

;; Simple is Emacs's built-in miscellaneous package.
(use-feature simple
  :delight visual-line-mode
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
  (global-visual-line-mode))

(use-feature tramp
  :defer
  :custom
  (tramp-default-method "ssh")
  (tramp-ssh-controlmaster-options
   (concat
    "-o ControlMaster=auto "
    "-o ControlPath='tramp.%%C' "
    ;; This defaults to no.
    "-o ControlPersist=yes")))

(use-feature files
  :custom
  (find-file-visit-truename t)
  (confirm-kill-emacs 'y-or-n-p)
  (confirm-nonexistent-file-or-buffer t)
  (save-abbrevs 'silently)
  (require-final-newline t)
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 8)
  (kept-old-versions 4)
  (version-control t)
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
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
  :straight (caml :files ("emacs/*.el")
                  :host github :repo "ocaml/ocaml" :branch "trunk"))

(use-package tuareg) ; for OCaml

(use-package cmake-mode
  :bind (:map cmake-mode-map ([remap xref-find-definitions] . cmake-help-command)))

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

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package ssh-config-mode)

(use-package toml-mode)

(use-package web-mode)

(use-package yaml-mode)

;;; Finish Loading:
(server-start)
(provide 'init)

;;; init.el ends here
