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

;;; Cursor and Mark Movement:
(bind-key "M-i" 'imenu)
(bind-key* "M-o" 'other-window)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

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

(use-package swiper
  :bind (("M-s s" . swiper)))

(use-package windmove
  :init (windmove-default-keybindings))

;;; Minibuffer Interface:
(use-package eldoc
  :delight
  :straight nil)

(use-package minibuf-eldef
  :straight nil
  :custom (minibuffer-eldef-shorten-default t)
  :config (minibuffer-electric-default-mode))

(use-package amx) ; fork of `smex', sorts `counsel-M-x'

(use-package counsel
  :delight
  :init (counsel-mode)
  :bind
  ;; Note that `counsel-mode' rebinds most commands.
  (([remap list-buffers] . counsel-ibuffer)
   ([remap imenu] . counsel-imenu)
   ([remap yank-pop] . counsel-yank-pop) ; browse kill ring
   ;; Browses the mark ring. Similar to `pop-global-mark' on `C-x C-SPC'.
   ("C-c C-SPC" . counsel-mark-ring)
   ("C-x L" . counsel-locate)
   ;; TODO: Maybe replace `projectile'.
   ;; https://www.reddit.com/r/emacs/comments/407q2c/ivy_is_now_available_in_spacemacs/cys6nts/
   ("C-c f" . counsel-git)
   ("M-s M-s" . counsel-rg)
   ("C-c r" . counsel-recentf)
   ("C-h L" . counsel-find-library))
  :custom
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp "\(?:\‘[#.]\)\|\(?:[#~]\’\)"))

(use-package flx) ; sorts `ivy' candidates

(use-package ivy
  :delight
  :init (ivy-mode)
  :bind (("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("C-o" . ivy-dispatching-done))
  :custom
  (ivy-height 6)
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
  (magit-completing-read-function #'ivy-completing-read)
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
  :commands (ibuffer))

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
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-indexing-method 'alien "Disable native indexing on Windows.")
  :config (projectile-mode))

(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode))

(use-package dired
  :straight nil
  :commands (dired)
  :custom
  (dired-dwim-target t "Enable side-by-side `dired' buffer targets.")
  (dired-recursive-copies 'always "Better recursion in `dired'.")
  (dired-recursive-deletes 'top)
  (dired-listing-switches "-lahp"))

(use-package recentf
  :straight nil
  :custom (recentf-max-saved-items 256)
  :config (recentf-mode))

(use-package rg ; `ripgrep'
  :bind (("M-s r" . rg)
         ("M-s d" . rg-dwim))
  :commands (rg-project rg-literal))

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

;;; Completion / syntax / tags:
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
  :hook (c-mode-common . lsp-cquery-enable)
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

(use-package ivy-xref
  :after ivy
  :custom (xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; Spelling
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
   (lambda nil
     (let ((red       "#dc322f")
           (magenta   "#d33682")
           (blue      "#268bd2")
           (cyan      "#2aa198")
           (green     "#859900")
           (base      "#839496"))
       ;; TODO: Make this macro "local."
       (defmacro with-face (str &rest properties)
         `(propertize ,str 'face (list ,@properties)))
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

(use-package delsel
  :straight nil
  :config (delete-selection-mode))

;; Simple is Emacs's built-in miscellaneous package.
(use-package simple
  :straight nil
  :custom
  ;; Fix kill behavior.
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

;; Enables undo/redo of windows configurations with 'C-c <left/right>'.
(winner-mode)

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

(use-package groovy-mode
  :mode (("\\.g\\(?:ant\\|roovy\\|radle\\)\\'" . groovy-mode)
         ("Jenkinsfile" . groovy-mode)))

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

(use-package toml-mode
  :mode ("\\.toml\\'"))

(use-package yaml-mode
  :mode "\\.ya?ml\'")

;;; Finish loading
(server-start)
(provide 'init)

;;; init.el ends here
