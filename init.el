;;; init --- Andrew Schwartzmeyer's Emacs init file
;;; Commentary:
;; See readme.

;;; Code:
(customize-set-variable 'gc-cons-threshold (* 10 1024 1024))

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

;;; Movement:
(use-package ace-window
  :bind ("M-o" . ace-window)
  :custom (aw-keys '(?a ?s ?d ?f ?g ?h ?1 ?2 ?3)))

(use-package buffer-move
  :bind (("C-S-<up>" . buf-move-up)
         ("C-S-<down>" . buf-move-down)
         ("C-S-<left>" . buf-move-left)
         ("C-S-<right>" . buf-move-right)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;;; Version control:
(use-package vc-hooks
  :straight nil
  :custom (vc-follow-symlinks t))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :custom
  (magit-completing-read-function #'ivy-completing-read)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-save-repository-buffers 'dontask)
  :config
  (magit-define-popup-switch 'magit-push-popup ?u
    "Set upstream" "--set-upstream"))

(use-package git-commit
  :config (global-git-commit-mode))

(use-package git-gutter
  :delight
  :config (global-git-gutter-mode))

;;; Interface:
(use-package avy
  :bind (("C-'" . avy-goto-char)
         ("M-g f" . avy-goto-line)))

(use-package minibuf-eldef
  :straight nil
  :custom (minibuffer-eldef-shorten-default t)
  :config (minibuffer-electric-default-mode))

(use-package mb-depth
  :straight nil
  :custom
  (enable-recursive-minibuffers
   t "Enables using `swiper-query-replace' from `swiper' via `M-q'.")
  :config (minibuffer-depth-indicate-mode))

;; Provides sorting for `counsel-M-x'.
(use-package amx)

(use-package counsel
  :delight
  :init (counsel-mode)
  :bind
  ;; Note that `counsel-mode' rebinds most commands.
  (;; Originally on `M-y', browses the kill ring.
   ([remap yank-pop] . counsel-yank-pop)
   ([remap list-buffers] . counsel-ibuffer)
   ;; Browses the mark ring. Similar to `pop-global-mark' on `C-x C-SPC'.
   ("C-c C-SPC" . counsel-mark-ring)
   ("C-x L" . counsel-locate)
   ;; TODO: Maybe replace `projectile'.
   ;; https://www.reddit.com/r/emacs/comments/407q2c/ivy_is_now_available_in_spacemacs/cys6nts/
   ("C-c f" . counsel-git)
   ("C-c k" . counsel-rg)
   ("C-c i" . counsel-imenu)
   ("C-h L" . counsel-find-library))
  :custom
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp "\(?:\‘[#.]\)\|\(?:[#~]\’\)"))

;; provides sorting for ivy
(use-package flx)

;; used with `C-o' in ivy
(use-package hydra)

(use-package ivy
  :delight
  :init (ivy-mode)
  :bind (("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("C-r" . ivy-previous-line-or-history)
         ("M-x" . ivy-reverse-i-search))
  :custom
  (ivy-re-builders-alist
   '(;; Use regex-plus but without ordering for files.
     (t . ivy--regex-ignore-order)))
  (ivy-use-virtual-buffers
   t "Add `recentf-mode' and bookmarks to `ivy-switch-buffer'.")
  (ivy-use-selectable-prompt t "Press `C-p' to use input as-is.")
  (ivy-initial-inputs-alist nil "Don't start with '^'.")
  :custom-face
  ;; Add a Solarized Green underline to the ivy match.
  ;; TODO: Consider `:inherit magit-diff-added-highlight'.
  (ivy-current-match ((t (:underline (:color "#859900"))))))

(use-package ivy-hydra
  :after (ivy hydra))

;; used to apply edits from `ivy-occur' buffers
(use-package wgrep
  :custom (wgrep-auto-save-buffer t))

(use-package which-key
  :delight
  :config (which-key-mode))

;;; Navigation:
(use-package projectile
  :delight '(:eval (concat " (" (projectile-project-name) ")"))
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-indexing-method 'alien "Disable native indexing on Windows.")
  :config (projectile-mode))

;;; Formatting:
(customize-set-variable 'indent-tabs-mode nil)
(customize-set-variable 'sentence-end-double-space nil)

(use-package dtrt-indent
  :delight
  :custom (dtrt-indent-min-quality 60)
  :config
  (add-to-list
   'dtrt-indent-hook-mapping-list
   '(powershell-mode c/c++/java powershell-indent))
  (dtrt-indent-global-mode))

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package adaptive-wrap
  :config (adaptive-wrap-prefix-mode))

(use-package whitespace
  :commands (whitespace-mode))

(use-package ws-butler
  :delight
  :custom (ws-butler-keep-whitespace-before-point nil)
  :config (ws-butler-global-mode))

;;; Editing:
(use-package autorevert
  :straight nil
  :delight auto-revert-mode
  :config (global-auto-revert-mode))

(use-package editorconfig
  :delight
  :config (editorconfig-mode))

(use-package saveplace
  :config
  (or (call-if-fbound #'save-place-mode)
      (call-if-fbound #'save-place)))

(use-package smartparens
  :delight
  :custom
  (sp-override-key-bindings '(("C-M-a" . nil)
                              ("C-M-e" . nil)
                              ("M-<backspace> " . nil)))
  (sp-base-key-bindings 'sp)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode))

(use-package undo-tree
  :delight
  :custom (undo-tree-auto-save-history t)
  :config (global-undo-tree-mode))

(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))

;;; Completion / syntax / tags:
(customize-set-variable 'tab-always-indent 'complete)

(use-package company
  :delight
  :bind
  (;; Originally `TAB', first indents, then completes.
   ;; https://github.com/company-mode/company-mode/issues/94
   ([remap indent-for-tab-command] . company-indent-or-complete-common)
   ([remap c-indent-line-or-region] . company-indent-or-complete-common)
   ;; Originally `C-M-i'.
   ([remap completion-at-point] . company-complete)
   :map company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ([remap company-complete-common] . company-complete-common-or-cycle))
  :custom
  (company-idle-delay nil)
  (company-tooltip-limit 7)
  :config (global-company-mode))

(use-package company-flx
  :after (company)
  :config (company-flx-mode))

(use-package company-quickhelp
  :after (company)
  :config (company-quickhelp-mode))

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
  :hook (c-mode-common . lsp-cquery-enable)
  :custom
  (cquery-executable (no-littering-expand-var-file-name "cquery/build/cquery"))
  (cquery-extra-init-params '(:completion (:detailedLabel t))))

(use-package company-lsp
  :after (cquery company lsp-mode)
  :custom (company-lsp-enable-recompletion t)
  :config (add-to-list 'company-backends 'company-lsp))

(use-package ivy-xref
  :after ivy
  :custom (xref-show-xrefs-function #'ivy-xref-show-xrefs))

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
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra")))

(use-package flyspell-correct-ivy
  :after (flyspell ivy))

(use-package auto-correct
  :delight
  :hook (flyspell-mode . auto-correct-mode))

;;; Language modes:
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.vcsh\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'magic-mode-alist '(";;; " . emacs-lisp-mode))

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package csharp-mode
  :mode "\\.cs$"
  :custom (csharp-want-imenu nil))

(use-package dockerfile-mode
  :mode "Dockerfile.*\\'")

(use-package fish-mode
  :mode ("\\.fish\\'" "/fish_funced\\..*\\'")
  :interpreter "fish")

(use-package gitattributes-mode
  :mode ("/\\.gitattributes\\'"
         "/info/attributes\\'"
         "/git/attributes\\'"))

(use-package gitconfig-mode
  :mode ("/\\.gitconfig\\'"
         "/\\.git/config\\'"
         "/modules/.*/config\\'"
         "/git/config\\'"
         "/\\.gitmodules\\'"
         "/etc/gitconfig\\'"))

(use-package gitignore-mode
  :mode ("/\\.gitignore\\'"
         "/info/exclude\\'"
         "/git/ignore\\'"))

(use-package json-mode
  :mode ("\\.json$" "\\.jsonld$"))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom (markdown-command "multimarkdown"))

(use-package nginx-mode
  :mode ("nginx\\.conf\\'" "/nginx/.+\\.conf\\'"))

(use-package powershell
  :mode ("\\.ps[dm]?1\\'" . powershell-mode))

(use-package protobuf-mode
  :mode "\\.proto\\'")

(use-package puppet-mode
  :mode "\\.pp\\'")

(use-package ruby-mode
  :mode "\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'")

(use-package rust-mode
  :mode "\\.rs\\'"
  :custom (rust-format-on-save t))

(use-package flycheck-rust
  :after (rust-mode flycheck)
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package ssh-config-mode
  :mode (("/\\.ssh/config\\'" . ssh-config-mode)
         ("/sshd?_config\\'" . ssh-config-mode)
         ("/known_hosts\\'" . ssh-known-hosts-mode)
         ("/authorized_keys2?\\'" . ssh-authorized-keys-mode)))

(use-package yaml-mode
  :mode "\\.ya?ml\'")

;;; Tools:
(use-package clang-format
  :after cc-mode
  ;; Does not use `:bind' in order to not delay loading `clang-format' indefinitely.
  :config (bind-key "C-M-\\" #'clang-format-region c-mode-base-map))

(use-package compile
  :straight nil
  :bind (("C-c c" . compile)
         ("M-O" . show-compilation))
  :custom
  (compilation-ask-about-save nil)
  (compilation-scroll-output t)
  (compilation-always-kill t))

(use-package demangle-mode
  :commands (demangle-mode))

(use-package ibuffer
  :commands (ibuffer))

(use-package ibuffer-vc
  :after ibuffer)

(use-package org
  :straight org-plus-contrib
  :mode (("\\.org\\'" . org-mode) ("[0-9]\\{8\\}\\'" . org-mode))
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

;; interactive ripgrep query
(use-package rg
  :commands (rg rg-project rg-dwim rg-literal))

(use-package swiper
  :bind (([remap isearch-backward] . swiper)
         ([remap isearch-forward] . swiper)))

(use-package yasnippet
  :delight yas-minor-mode
  :bind-keymap ("C-c &" . yas-keymap)
  :config (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package auto-yasnippet
  :after yasnippet
  :commands (aya-create))

;;; Appearance:
(if (display-graphic-p)
    (progn
      (tool-bar-mode 0)
      (scroll-bar-mode 0)))

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

(use-package hl-todo
  :config (global-hl-todo-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package fortune-cookie
  :if (executable-find "fortune")
  :custom
  (fortune-cookie-fortune-args "-s")
  (fortune-cookie-cowsay-enable (executable-find "cowsay"))
  (fortune-cookie-cowsay-args "-f tux")
  :config (fortune-cookie-mode))

(use-package smooth-scrolling
  :delight
  :custom (smooth-scroll-margin 2)
  :config (smooth-scrolling-mode))

(use-package uniquify
  :straight nil
  :custom (uniquify-buffer-name-style 'forward))

;;; Emacs configuration:

;; Fix annoyances.
(customize-set-variable 'delete-by-moving-to-trash t)
(customize-set-variable 'ring-bell-function 'ignore)
(customize-set-variable 'visible-bell t)
(customize-set-variable 'inhibit-startup-screen t)
(customize-set-variable 'load-prefer-newer t)
(defalias 'yes-or-no-p 'y-or-n-p)
(set-variable 'disabled-command-function nil)

;; Default to truncating lines.
(customize-set-variable 'truncate-lines t)

;; Simple is Emacs's built-in miscellaneous package.
(use-package simple
  :straight nil
  :custom
  ;; Fix kill behavior.
  (save-interprogram-paste-before-kill t)
  (kill-do-not-save-duplicates t)
  (kill-whole-line t)
  :config (column-number-mode))

;; Pop repeatedly.
(customize-set-variable 'set-mark-command-repeat-pop t)

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

(use-package recentf
  :straight nil
  :custom (recentf-max-saved-items 256)
  :config (recentf-mode))

(use-package dired
  :straight nil
  :commands (dired)
  :custom
  (dired-dwim-target t "Enable side-by-side `dired' buffer targets.")
  (dired-recursive-copies 'always "Better recursion in `dired'.")
  (dired-recursive-deletes 'top)
  (dired-listing-switches "-lahp"))

;; Enables undo/redo of windows configurations with 'C-c <left/right>'.
(winner-mode)

;; Start the Emacs daemon.
(server-start)

;;; provide init package
(provide 'init)

;;; init.el ends here
