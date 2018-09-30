;;; init --- Andrew Schwartzmeyer's Emacs init file
;;; Commentary:
;; See readme.

;;; Code:
(customize-set-variable 'gc-cons-threshold 20000000) ; 20 MB

;; Default to UTF-8 early as this file uses Unicode symbols.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

;; used for deprecated functions
(defun call-if-fbound (function &rest args)
  "Call FUNCTION with optional ARGS, only if it is `fbound'."
  "Return t if it is fbound and called without error, and nil otherwise."
  (when (fboundp function) (apply function args) t))

;;; Package:
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 4))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(customize-set-variable 'straight-use-package-by-default t)

(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package))

(use-package benchmark-init
  :init (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate))

(use-package delight)
(use-package bind-key)
(use-package dash)
(use-package f)

(push "~/.emacs.d/lisp" load-path)

;; Save data files consistently:
;; - `save-place-file'
;; - `undo-tree-history-directory-alist'
;; - `backup-directory-alist'
;; - etc.
(use-package no-littering)

(customize-set-variable
 'custom-file (no-littering-expand-var-file-name "custom.el"))

;; Colorize strings:
(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

;;; Platform:
(use-package linux
  :straight nil
  :if (eq system-type 'gnu/linux))

(use-package osx
  :straight nil
  :if (eq system-type 'darwin))

(use-package windows
  :straight nil
  :if (eq system-type 'windows-nt))

;;; Cursor and Mark Movement:
(bind-key "M-i" #'imenu)
(bind-key* "M-o" #'other-window)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package smartparens
  :delight
  :hook (emacs-lisp-mode . smartparens-strict-mode)
  :custom (sp-wrap-repeat-last 2 "Always repeat")
  ;; Remap Emacs equivalent commands
  :bind (:map smartparens-mode-map
              ([remap forward-sexp]       . sp-forward-sexp)
              ([remap backward-sexp]      . sp-backward-sexp)
              ([remap forward-list]       . sp-next-sexp)
              ([remap backward-list]      . sp-previous-sexp)
              ([remap down-list]          . sp-down-sexp)
              ([remap backward-up-list]   . sp-backward-up-sexp)
              ([remap transpose-sexps]    . sp-transpose-sexp)
              ([remap kill-sexp]          . sp-kill-sexp)
              ([remap backward-kill-word] . sp-backward-kill-word)
              ([remap mark-sexp]          . sp-mark-sexp)
              ("C-M-m"                    . sp-copy-sexp) ; also M-RET
              ("C-M-<backspace>"          . sp-splice-sexp-killing-backward))
  :config
  (require 'smartparens-config)
  (smartparens-global-mode))

(use-package subword
  :straight nil
  :init (global-subword-mode))

(use-package windmove
  :init (windmove-default-keybindings))

(use-package winner
  ;; Enables undo/redo of windows configurations with
  ;; `C-c <left/right>'.
  :straight nil
  :init (winner-mode))

;;; Minibuffer Interface:
(bind-key "C-h L" #'find-library)

(use-package eldoc
  :delight
  :straight nil)

(use-package minibuf-eldef
  :custom (minibuffer-eldef-shorten-default t)
  :config (minibuffer-electric-default-mode))

(use-package ido
  :straight nil
  :custom
  (ido-use-virtual-buffers t)
  (ido-use-filename-at-point 'guess)
  (ido-everywhere t)
  :config (ido-mode))

(use-package ido-completing-read+
  :config (ido-ubiquitous-mode))

(use-package ido-complete-space-or-hyphen
  :config (ido-complete-space-or-hyphen-mode))

(use-package flx-ido
  :custom (ido-enable-flex-matching t)
  :config (flx-ido-mode))

(use-package amx ; fork of `smex'
  :custom
  (amx-history-length 20)
  (amx-prompt-string (with-face "> " :foreground "#2aa198"))
  (amx-show-key-bindings nil)
  :config (amx-mode))

(use-package counsel
  :delight
  :bind (("C-c C-y" . counsel-yank-pop) ; browse kill ring
         ("C-c C-SPC" . counsel-mark-ring)
         ([remap bookmark-jump] . counsel-bookmark)))

(use-package which-key
  :delight
  :config (which-key-mode))

;;; Version Control:
(use-package vc-hooks
  :straight nil
  :custom (vc-follow-symlinks t))

(use-package magit
  :straight (magit :host github :repo "magit/magit" :branch "maint")
  :demand
  :custom
  (magit-completing-read-function #'magit-ido-completing-read)
  (magit-save-repository-buffers 'dontask)
  (magit-published-branches nil "Disable confirmation.")
  ;; TODO: Maybe `(magit-dwim-selection '((magit-branch-and-checkout nil t)))'
  :config
  (magit-define-popup-switch 'magit-push-popup ?u
    "Set upstream" "--set-upstream"))

(use-package git-commit
  :config (global-git-commit-mode))

(use-package git-gutter
  :delight
  :config (global-git-gutter-mode))

;;; Buffers:
(use-package buffer-move
  :bind (("C-S-<up>" . buf-move-up)
         ("C-S-<down>" . buf-move-down)
         ("C-S-<left>" . buf-move-left)
         ("C-S-<right>" . buf-move-right)))

(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer))

(use-package ibuffer-vc
  :after ibuffer)

(use-package midnight
  ;; Kill old buffers at midnight.
  :straight nil
  :config (midnight-mode))

(use-package uniquify
  :straight nil
  :custom (uniquify-buffer-name-style 'forward))

;;; File Navigation:
(use-package projectile
  :delight '(:eval (concat " (" (projectile-project-name) ")"))
  :bind-keymap (("C-;" . projectile-command-map))
  :custom
  (projectile-indexing-method 'turbo-alien "Use Git")
  (projectile-git-submodule-command nil "Ignore submodules")
  :config (projectile-mode))

(use-package dired
  :straight nil
  :commands (dired)
  :custom
  (dired-dwim-target t "Enable side-by-side `dired' buffer targets.")
  (dired-recursive-copies 'always "Better recursion in `dired'.")
  (dired-recursive-deletes 'top)
  (dired-listing-switches "-lahp"))

(use-package dired-x
  :straight nil
  :bind (("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window))
  :init (add-hook 'dired-mode-hook (lambda () (require 'dired-x))))

(use-package recentf
  :straight nil
  :custom (recentf-max-saved-items 256)
  :config (recentf-mode))

(use-package rg ; `ripgrep'
  :bind (("M-s r" . rg)
         ("M-s s" . rg-dwim))
  :commands (rg-project rg-literal))

(use-package wdired
  :straight nil
  :custom (wdired-allow-to-change-permissions t))

(use-package wgrep
  ;; makes `rg' buffers writable too
  :custom (wgrep-auto-save-buffer t))

;;; Formatting / Indentation / Whitespace:
(customize-set-variable 'indent-tabs-mode nil)
(customize-set-variable 'sentence-end-double-space nil)

(use-package clang-format
  :after cc-mode
  ;; Does not use `:bind' in order to not delay loading `clang-format' indefinitely.
  :config (bind-key [remap indent-region] #'clang-format-region c-mode-base-map))

(use-package dtrt-indent
  :delight
  :custom (dtrt-indent-min-quality 60)
  :config
  (add-to-list
   'dtrt-indent-hook-mapping-list
   '(powershell-mode c/c++/java powershell-indent))
  (dtrt-indent-global-mode))

(use-package editorconfig
  :delight
  :config (editorconfig-mode))

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package whitespace
  :commands (whitespace-mode))

(use-package ws-butler
  :delight
  :custom (ws-butler-keep-whitespace-before-point nil)
  :config (ws-butler-global-mode))

;;; Editing:
(customize-set-variable 'truncate-lines t)
(bind-key "C-x w" #'toggle-truncate-lines)
(bind-key [remap zap-to-char] #'zap-up-to-char)

(use-package autorevert
  :straight nil
  :delight auto-revert-mode
  :custom (auto-revert-verbose nil)
  :config (global-auto-revert-mode))

(use-package saveplace
  :config
  (or (call-if-fbound #'save-place-mode)
      (call-if-fbound #'save-place)))

(use-package yasnippet
  :delight yas-minor-mode
  :bind-keymap ("C-c &" . yas-keymap)
  :config (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package auto-yasnippet
  :after yasnippet
  :commands (aya-create))

(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))

;;; Completion / Syntax / Tags:
(customize-set-variable 'tab-always-indent 'complete)

(use-package flycheck
  :delight
  :bind-keymap ("C-c !" . flycheck-mode-map)
  :config (global-flycheck-mode))

;; options include irony, cquery, rtags, ggtags, and ycmd
(use-package lsp-mode
  :defer t
  :custom-face
  (lsp-face-highlight-textual ((t (:background unspecified))))
  ;; Solarized Red
  (lsp-face-highlight-read ((t (:background "#DC322F"))))
  ;; Solarized Cyan
  (lsp-face-highlight-write ((t (:background "#2AA198")))))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package cquery
  :commands (lsp-cquery-enable)
  :hook (c-mode-common . (lambda ()
                           (or
                            (boundp 'cquery-enabled)
                            (when (setq cquery-enabled
                                        (y-or-n-p "Start cquery?"))
                              (lsp-cquery-enable)))))
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

;;; Spelling:
(use-package flyspell
  ;; Disable on Windows because `aspell' 0.6+ isn't available.
  :if (not (eq system-type 'windows-nt))
  :delight
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :custom
  ;; NOTE: This unbinds `C-M-i' because it's interpreted as `M-TAB'.
  ;; This is because `C-i' and `TAB' are the same character in ASCII.
  ;;
  ;; https://www.gnu.org/software/emacs/elisp/html_node/Function-Keys.html
  (flyspell-use-meta-tab nil)
  (flyspell-mode-map (make-sparse-keymap))
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra")))

(use-package auto-correct
  :delight
  :hook (flyspell-mode . auto-correct-mode))

;;; Tools:
(use-package compile
  :straight nil
  :bind (("C-c c" . compile))
  :custom
  (compilation-ask-about-save nil)
  (compilation-scroll-output t)
  (compilation-always-kill t))

(use-package demangle-mode
  :commands (demangle-mode))

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

(use-package org
  :straight org-plus-contrib
  :hook (org-mode . turn-on-auto-fill)
  :custom
  (org-startup-indented nil)
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

(use-package re-builder
  :straight nil
  :commands (re-builder regexp-builder)
  :custom (reb-re-syntax 'string))

(use-package restart-emacs
  :bind ("C-c Q" . restart-emacs))

;;; Appearance:
(if (display-graphic-p)
    (progn
      (tool-bar-mode 0)
      (scroll-bar-mode 0)))

(use-package fortune-cookie
  :if (executable-find "fortune")
  :custom
  (fortune-cookie-fortune-args "-s")
  (fortune-cookie-cowsay-enable (executable-find "cowsay"))
  (fortune-cookie-cowsay-args "-f tux")
  :config (fortune-cookie-mode))

(use-package hl-todo
  :config (global-hl-todo-mode))

(use-package paren
  :straight nil
  :custom (show-paren-delay 0)
  :init (show-paren-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smooth-scrolling
  :delight
  :custom (smooth-scroll-margin 2)
  :config (smooth-scrolling-mode))

(use-package solarized-theme
  :if (display-graphic-p)
  :custom (solarized-use-variable-pitch nil)
  :config
  (defun toggle-theme ()
    "Switch between Solarized variants."
    (interactive)
    (load-theme (if (eq (car custom-enabled-themes) 'solarized-dark)
                    'solarized-light 'solarized-dark)
                t))
  (load-theme 'solarized-dark t))

(if (not (display-graphic-p))
    (load-theme 'tango-dark t))

;;; Internal Emacs configuration:

;; Fix annoyances.
(customize-set-variable 'minibuffer-message-timeout 0.5)
(customize-set-variable 'set-mark-command-repeat-pop t)
(customize-set-variable 'delete-by-moving-to-trash t)
(customize-set-variable 'ring-bell-function 'ignore)
(customize-set-variable 'visible-bell t)
(customize-set-variable 'inhibit-startup-screen t)
(customize-set-variable 'load-prefer-newer t)
(defalias 'yes-or-no-p 'y-or-n-p)
(set-variable 'disabled-command-function nil)

;; Simple is Emacs's built-in miscellaneous package.
(use-package simple
  :straight nil
  :custom
  (mark-ring-max 1024)
  (global-mark-ring-max 1024)
  (save-interprogram-paste-before-kill t)
  (kill-do-not-save-duplicates t)
  (kill-whole-line t)
  (shift-select-mode nil)
  :init
  (column-number-mode))

(use-package files
  :straight nil
  :custom
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
  (large-file-warning-threshold (* 20 1000 1000) "20 megabytes."))

;;; Language modes:
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.vcsh\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'magic-mode-alist '(";;; " . emacs-lisp-mode))

(use-package apt-sources-list)

(use-package cmake-mode)

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
  :hook (markdown-mode . auto-fill-mode)
  :custom (markdown-command "multimarkdown"))

(use-package nginx-mode)

(use-package powershell)

(use-package protobuf-mode)

(use-package puppet-mode)

(use-package ruby-mode)

(use-package rust-mode
  :custom (rust-format-on-save t))

(use-package flycheck-rust
  :after (rust-mode flycheck)
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package ssh-config-mode)

(use-package toml-mode)

(use-package yaml-mode)

;;; Finish loading
(server-start)
(provide 'init)

;;; init.el ends here
