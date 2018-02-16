;;; init --- Andrew Schwartzmeyer's Emacs init file
;;; Commentary:
;; See readme.

;;; Code:

;;; Package:
(setq gc-cons-threshold (* 10 1024 1024))

(setq package-check-signature nil)
(require 'package)
(setq load-prefer-newer t
      package-enable-at-startup nil
      package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))

(require 'diminish)
(require 'bind-key)

(use-package dash)
(use-package f)

;;; Platform:
(use-package linux
  :ensure nil
  :load-path "lisp/"
  :if (eq system-type 'gnu/linux))

(use-package osx
  :ensure nil
  :load-path "lisp/"
  :if (eq system-type 'darwin))

(use-package windows
  :ensure nil
  :load-path "lisp/"
  :if (eq system-type 'windows-nt))

;;; Vim:
(use-package evil
  :config
  (setq evil-want-C-u-scroll t
        evil-want-fine-undo 'no
        evil-cross-lines t
        evil-disable-insert-state-bindings t)
  (evil-mode)
  (add-hook 'git-commit-mode-hook 'evil-insert-state))

;; required by evil
(use-package goto-chg
  :commands (goto-last-change goto-last-change-reverse))

(use-package evil-args
  :bind (;; bind evil-args text objects
         :map evil-inner-text-objects-map
              ("a" . evil-inner-arg)
              :map evil-outer-text-objects-map
              ("a" . evil-outer-arg)

              ;; bind evil-forward/backward-args
              :map evil-normal-state-map
              ("L" . evil-forward-arg)
              ("H" . evil-backward-arg)
              ("L" . evil-forward-arg)
              ("H" . evil-backward-arg)

              ;; bind evil-jump-out-args
              :map evil-normal-state-map
              ("K" . evil-jump-out-args)))

(use-package evil-commentary
  :diminish evil-commentary-mode
  :config (evil-commentary-mode))

(use-package evil-escape
  :diminish evil-escape-mode
  :bind ("C-c C-g" . evil-escape)
  :init
  (setq evil-escape-key-sequence "jk")
  (evil-escape-mode))

(use-package evil-matchit
  :config (global-evil-matchit-mode))

(use-package evil-numbers
  :bind (:map evil-normal-state-map
              ("C-c +" . evil-numbers/inc-at-pt)
              ("C-c -" . evil-numbers/dec-at-pt)
              :map evil-visual-state-map
              ("C-c +" . evil-numbers/inc-at-pt)
              ("C-c -" . evil-numbers/dec-at-pt)))

(use-package evil-surround
  :config (global-evil-surround-mode))

(use-package evil-visualstar
  :config (global-evil-visualstar-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;;; Version control:
(setq vc-follow-symlinks t)
(use-package magit
  :bind ("C-c g" . magit-status)
  :commands (magit-status projectile-vc)
  :config
  (use-package evil-magit)
  (add-to-list 'magit-log-arguments "--no-abbrev-commit")
  (setq magit-popup-use-prefix-argument 'default
        magit-completing-read-function 'ivy-completing-read))
(global-git-commit-mode)

(use-package git-gutter
  :diminish git-gutter-mode
  :config (global-git-gutter-mode))

;;; Interface:
(use-package smex)
(use-package counsel
  :diminish counsel-mode
  :bind
  ;; note that counsel-mode rebinds most commands
  (("C-s"     . counsel-grep-or-swiper)
   ("C-x l"   . counsel-locate)
   ("C-c k"   . counsel-rg)
   ("C-c i"   . counsel-imenu))
  :config
  (setq counsel-find-file-at-point t)
  (setq counsel-find-file-ignore-regexp "\\.DS_Store\\|.git")
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  (counsel-mode))

(use-package ivy
  :diminish ivy-mode
  :bind (("C-c C-r" . ivy-resume))
  :config
  (ivy-mode)
  (setq enable-recursive-minibuffers t)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; press "C-p" to use input as-is
  (setq ivy-use-selectable-prompt t)
  ;; don't start with '^'
  (setq ivy-initial-inputs-alist nil))

(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode))

(use-package buffer-move
  :commands (buf-move-up buf-move-down buf-move-left buf-move-right))

;;; Navigation:
(use-package projectile
  :diminish projectile-mode
  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'ivy)
  (projectile-global-mode))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;;; Formatting:
(setq-default indent-tabs-mode nil)
(setq sentence-end-double-space nil)

(use-package dtrt-indent
  :diminish dtrt-indent-mode
  :config
  (dtrt-indent-mode)
  (setq dtrt-indent-min-quality 60
        dtrt-indent-verbosity 3))

(use-package adaptive-wrap
  :config (adaptive-wrap-prefix-mode))

(use-package whitespace
  :commands (whitespace-mode)
  :config
  (setq whitespace-style '(face tabs spaces newline empty
                                trailing tab-mark newline-mark)))
(use-package ws-butler
  :diminish ws-butler-mode
  :config (ws-butler-global-mode))

;;; Editing:
(use-package smartparens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (show-smartparens-global-mode)
  (smartparens-global-strict-mode))

(use-package evil-smartparens
  :diminish evil-smartparens-mode
  :config (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :config (global-highlight-parentheses-mode))

(use-package hungry-delete
  :diminish hungry-delete-mode
  :config (global-hungry-delete-mode))

(use-package saveplace
  :config
  (setq save-place-file (f-expand "saved-places" user-emacs-directory))
  (save-place-mode))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist
        `(("." . ,(f-expand "undo-tree" user-emacs-directory)))
        undo-tree-auto-save-history t))

(use-package unfill
  :commands (unfill-region unfill-paragraph unfill-toggle))

;;; Completion / tags:
(use-package company
  :diminish company-mode
  :config
  (push '(company-clang :with company-semantic) company-backends)
  (global-company-mode))

(use-package company-statistics
  :config (company-statistics-mode))

;;; Syntax / spell checking:
(use-package flycheck
  :diminish flycheck-mode
  :config (global-flycheck-mode))

(use-package flyspell
  :diminish flyspell-mode
  :config
  (use-package auto-correct
    :config (add-hook 'flyspell-mode-hook 'auto-correct-mode))
  (setq ispell-program-name (executable-find "aspell")
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package uniquify
  :ensure nil
  :config (setq uniquify-buffer-name-style 'forward))

;;; Language modes:
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.vcsh\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'magic-mode-alist '(";;; " . emacs-lisp-mode))

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package csharp-mode
  :mode "\\.cs$"
  :config (setq csharp-want-imenu nil))

(use-package dockerfile-mode
  :mode "Dockerfile.*\\'")

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
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "multimarkdown")
  (evil-define-key 'normal markdown-mode-map
    (kbd "g d") 'markdown-jump
    (kbd "g x") 'markdown-follow-link-at-point))

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
  :config
  (use-package flycheck-rust)
  (setq rust-format-on-save t))

(use-package ssh-config-mode
  :mode (("/\\.ssh/config\\'" . ssh-config-mode)
         ("/sshd?_config\\'" . ssh-config-mode)
         ("/known_hosts\\'" . ssh-known-hosts-mode)
         ("/authorized_keys2?\\'" . ssh-authorized-keys-mode)))

(use-package yaml-mode
  :mode "\\.ya?ml\'")

;;; Tools:
(use-package clang-format
  :ensure nil
  :load-path "lisp/"
  :config (evil-define-key 'visual c++-mode-map "=" 'clang-format-region))

(use-package demangle-mode
  :commands demangle-mode)

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config (use-package ibuffer-vc))

(use-package org-plus-contrib
  :mode (("\\.org\\'" . org-mode) ("[0-9]\\{8\\}\\'" . org-mode))
  :config
  (use-package evil-org)
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (setq org-latex-listings t
        org-pretty-entities t
        org-latex-custom-lang-environments '((C "lstlisting"))
        org-entities-user '(("join" "\\Join" nil "&#9285;" "" "" "⋈")
                            ("reals" "\\mathbb{R}" t "&#8477;" "" "" "ℝ")
                            ("ints" "\\mathbb{Z}" t "&#8484;" "" "" "ℤ")
                            ("complex" "\\mathbb{C}" t "&#2102;" "" "" "ℂ")
                            ("models" "\\models" nil "&#8872;" "" "" "⊧"))
        org-export-backends '(html beamer ascii latex md)))

(use-package restart-emacs
  :bind ("C-c Q" . restart-emacs))

(use-package tramp
  :config
  (setq tramp-verbose 9
        tramp-default-method "ssh"
        tramp-ssh-controlmaster-options
        (concat "-o ControlPath=/tmp/tramp.%%r@%%h:%%p "
                "-o ControlMaster=auto "
                "-o ControlPersist=no")))

;;; Appearance:
(if (display-graphic-p)
    (progn
      (tool-bar-mode 0)
      (scroll-bar-mode 0)))

(use-package solarized-theme
  :config
  (setq solarized-use-variable-pitch nil)
  (load-theme 'solarized-dark t))

(use-package fortune-cookie
  :disabled t
  :config
  (setq fortune-cookie-fortune-args "-s"
        fortune-cookie-cowsay-args "-f tux")
  (fortune-cookie-mode))

(use-package smooth-scroll
  :if (display-graphic-p)
  :diminish smooth-scroll-mode
  :config
  (setq smooth-scroll/vscroll-step-size 8)
  (smooth-scroll-mode))

;;; Emacs configuration:
(setq next-screen-context-lines 4)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq calendar-week-start-day 1) ; Monday
(winner-mode)
(setq disabled-command-function nil)
(setq-default truncate-lines t)
(setq ring-bell-function 'ignore
      visible-bell t)
(setq inhibit-startup-message t)
(setq save-interprogram-paste-before-kill t
      kill-do-not-save-duplicates t
      kill-whole-line t)
(setq-default set-mark-command-repeat-pop t)
(setq system-uses-terminfo nil)
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 4
      kept-old-versions 2
      version-control t
      backup-directory-alist `(("." . ,(f-expand
                                        "backups" user-emacs-directory))))
(setq large-file-warning-threshold (* 20 1000 1000)) ; 20 MB
(setq recentf-max-saved-items 256
      recentf-max-menu-items 16)
(recentf-mode)
(global-auto-revert-mode)
(setq dired-dwim-target t ; enable side-by-side dired buffer targets
      dired-recursive-copies 'always ; better recursion in dired
      dired-recursive-deletes 'top
      dired-listing-switches "-lahp")
(setq compilation-ask-about-save nil
      compilation-always-kill t)

;;; provide init package
(provide 'init)

;;; init.el ends here
