;;; init --- Andrew Schwartzmeyer's Emacs init file
;;; Commentary:
;; See readme.

;;; Code:
;; Emacs Lisp prefer newer
(setq load-prefer-newer t)

;;; package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

(defun install-use-package ()
  (when (not (package-installed-p 'use-package))
    (package-install 'use-package)))

(condition-case nil
    (install-use-package)
  (error
   (package-refresh-contents)
   (install-use-package)))

(setq use-package-verbose t
      use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))

(use-package diminish)
(use-package bind-key)
(use-package dash)

(use-package auto-compile
  :config
  (progn
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)))

;; for f-expand
(use-package f)

;;; bindings
;; miscellaneous
(bind-key "C-c l" 'align-regexp)
(bind-key "C-c x" 'eval-buffer)
(bind-key "C-c q" 'auto-fill-mode)
(bind-key "C-c v" 'visual-line-mode)
(bind-key "C-c b" 'find-library)

;; isearch
(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "C-r" 'isearch-backward-regexp)
(bind-key "C-M-s" 'isearch-forward)
(bind-key "C-M-r" 'isearch-backward)

;; window management
(bind-key* "M-1" 'delete-other-windows)
(bind-key* "M-2" 'split-window-vertically)
(bind-key* "M-3" 'split-window-horizontally)
(bind-key* "M-0" 'delete-window)

;;; appearance
(if (display-graphic-p)
    (progn
      (tool-bar-mode 0)
      (scroll-bar-mode 0)))

;; Solarized
(use-package emacs-color-theme-solarized
  :ensure color-theme-solarized
  :init
  (progn
    (customize-set-variable 'frame-background-mode 'dark)
    (load-theme 'solarized t)))

;; Zenburn
(use-package zenburn-theme
  :disabled t
  :config (load-theme 'zenburn t))

;; smooth scrolling
(use-package smooth-scroll
  :if (display-graphic-p)
  :diminish smooth-scroll-mode
  :config
  (progn
    (setq smooth-scroll/vscroll-step-size 8)
    (smooth-scroll-mode)))


;; line/column numbers in mode-line
(line-number-mode)
(column-number-mode)

;; status
(display-time-mode)
(display-battery-mode)

;; y/n for yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; quit prompt
(setq confirm-kill-emacs 'y-or-n-p)

;; start week on Monday
(setq calendar-week-start-day 1)

;; cursor settings
(blink-cursor-mode)

;; visually wrap lines
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; matching parentheses
(show-paren-mode)

;; window undo/redo
(winner-mode)

;;; settings
;; enable all commands
(setq disabled-command-function nil)

;; default truncate lines
(setq-default truncate-lines t)

;; disable bell
(setq ring-bell-function 'ignore
      visible-bell t)

;; increase garbage collection threshold
(setq gc-cons-threshold 20000000)

;; inhibit startup message
(setq inhibit-startup-message t)

;; kill settings
(setq save-interprogram-paste-before-kill t
      kill-append-merge-undo t
      kill-do-not-save-duplicates t
      kill-whole-line t)

;; remove selected region if typing
(delete-selection-mode)

;; repeat mark pop
(setq-default set-mark-command-repeat-pop t)

;; prefer UTF8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; set terminfo
(setq system-uses-terminfo nil)

;;; files
;; backups
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 4
      kept-old-versions 2
      version-control t
      backup-directory-alist `(("." . ,(f-expand
                                        "backups" user-emacs-directory))))

;; recent files
(setq large-file-warning-threshold 100000000)

(setq recentf-max-saved-items 256)
(recentf-mode)

;; final-newline
(setq require-final-newline 't)

;; set auto revert of buffers if file is changed externally
(global-auto-revert-mode)

;; symlink version-control follow
(setq vc-follow-symlinks t)

;; add more modes
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.vcsh\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'magic-mode-alist '(";;; " . emacs-lisp-mode))

;; dired
(setq dired-dwim-target t ; enable side-by-side dired buffer targets
      dired-recursive-copies 'always ; better recursion in dired
      dired-recursive-deletes 'top
      dired-listing-switches "-lahp")

;; compilation
(setq compilation-ask-about-save nil
      compilation-always-kill t)

;;; functions
(defun compile-init ()
  "Byte recompile user Emacs directory."
  (interactive)
  (byte-recompile-directory user-emacs-directory))

(defun copy-buffer-file-path ()
  "Put current buffer's short path into the kill ring."
  (interactive)
  (when (buffer-file-name)
    (kill-new (f-short (buffer-file-name)))))

(defun copy-buffer-file-name ()
  "Put current buffer's base name into the kill ring."
  (interactive)
  (when (buffer-file-name)
    (kill-new (f-filename (buffer-file-name)))))

;; load Linux configuration
(use-package linux
  :ensure nil
  :load-path "lisp/"
  :if (eq system-type 'gnu/linux))

;; load OS X configurations
(use-package osx
  :ensure nil
  :load-path "lisp/"
  :if (eq system-type 'darwin))

;; load Windows configurations
(use-package windows
  :ensure nil
  :load-path "lisp/"
  :if (eq system-type 'windows-nt))

;;; load local settings
(use-package local
  :ensure nil
  :load-path "site-lisp/")

;;; extensions

;; ace-window
(use-package ace-window
  :bind* ("M-s" . ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; ag - the silver searcher
(use-package ag
  :commands (ag ag-files ag-regexp ag-project ag-dired helm-ag)
  :config (setq ag-highlight-search t
		ag-reuse-buffers t))

(use-package helm-ag
  :bind ("C-c s" . helm-ag))

(use-package aggressive-indent
  :config (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

;; Mercurial
(use-package ahg
  :commands ahg-status)

;; anzu - number of search matches in modeline
(use-package anzu
  :diminish anzu-mode
  :config (global-anzu-mode))

;; auto update packages
(use-package auto-package-update
  :config
  (progn
    (setq auto-package-update-interval 1)
    (when (and (apu--should-update-packages-p)
	       (not (string= (getenv "CI") "true"))
	       (y-or-n-p-with-timeout "Update packages?" 5 t))
      (auto-package-update-now))))

;; avy
(use-package avy
  :bind (("M-g M-g" . avy-goto-line)
	 ("C-." . avy-goto-char-2)))

;; bison
(use-package bison-mode
  :mode ("\\.y\\'" "\\.l\\'"))

;; add C style for work
(c-add-style "work"
	     '("bsd"
	       (c-basic-offset . 4)
	       (indent-tabs-mode . nil)))

;; CMake
(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; company "complete anything"
(use-package company
  :commands (company-mode)
  :config
  (progn
    (use-package company-c-headers)
    (push '(company-clang
	    :with company-semantic
	    :with company-yasnippet
	    :with company-c-headers)
          company-backends)
    (setq company-minimum-prefix-length 2
          company-idle-delay nil
	  company-global-modes '(not gud-mode))))

(use-package helm-company
  :bind ("<backtab>" . helm-company)
  :commands (helm-company)
  :config
  (progn
    (company-mode)
    (define-key company-mode-map (kbd "C-:") 'helm-company)
    (define-key company-active-map (kbd "C-:") 'helm-company)))

;; crontab
(use-package crontab-mode
  :mode "\\.cron\\(tab\\)?\\'")

;; C#
(use-package csharp-mode
  :mode "\\.cs$"
  :init (add-to-list 'c-default-style '(csharp-mode . "c#"))
  :config (add-hook 'csharp-mode-hook (lambda ()
					(progn
					  (setq indent-tabs-mode nil)
					  (set-fill-column 90)
					  (c-set-style "c#")))))

;; docker
(use-package docker
  :commands docker-mode)

(use-package dockerfile-mode
  :mode "Dockerfile.*\\'")

;; better killing
(use-package easy-kill
  :bind* (("M-w" . easy-kill)
	  ("C-M-@" . easy-mark)))

;; activate expand-region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; flycheck
(use-package flycheck
  :init (global-flycheck-mode))

(use-package helm-flycheck
  :bind ("C-c ! h" . helm-flycheck)
  :config (global-flycheck-mode))

;; flyspell - use aspell instead of ispell
(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :config (setq ispell-program-name (executable-find "aspell")
                ispell-extra-args '("--sug-mode=ultra")))

;; git modes
(use-package gitattributes-mode
  :disabled t)
(use-package gitconfig-mode
  :mode ("/\\.gitconfig\\'" "/\\.git/config\\'" "/git/config\\'" "/\\.gitmodules\\'"))
(use-package gitignore-mode
  :mode ("/\\.gitignore\\'" "/\\.git/info/exclude\\'" "/git/ignore\\'"))

;; gnuplot
(use-package gnuplot
  :commands (gnuplot-mode gnuplot-make-buffer))

;; handlebars
(use-package handlebars-mode
  :mode ("\\.handlebars$" "\\.hbs$"))

;; haskell
(use-package haskell-mode
  :mode "\\.\\(?:[gh]s\\|hi\\)\\'"
  :interpreter ("runghc" "runhaskell"))

;; helm
(use-package helm
  :diminish helm-mode
  :bind* (("M-x" . helm-M-x)
	  ("C-c M-x" . execute-extended-command)
	  ("M-]" . helm-command-prefix)
	  ("M-y" . helm-show-kill-ring)
	  ("C-x C-b" . helm-buffers-list)
	  ("C-x b" . helm-mini)
	  ("C-x C-f" . helm-find-files))
  :config
  (progn
    (require 'helm-config)
    (bind-key "C-c !" 'helm-toggle-suspend-update helm-map)
    (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
    (bind-key "C-i" 'helm-execute-persistent-action helm-map)
    (bind-key "C-z" 'helm-select-action helm-map)
    (setq helm-M-x-fuzzy-match t
	  helm-recentf-fuzzy-match t
	  helm-buffers-fuzzy-matching t
	  helm-semantic-fuzzy-match t
	  helm-imenu-fuzzy-match t
	  helm-apropos-fuzzy-match t
	  helm-lisp-fuzzy-completion t
	  helm-move-to-line-cycle-in-source t
	  helm-ff-file-name-history-use-recentf t
	  helm-ff-auto-update-initial-value nil
	  helm-tramp-verbose 9)
    (helm-mode)
    (helm-autoresize-mode t)))

;; ledger
(use-package ledger-mode
  :mode "\\.ledger\\'"
  :config (use-package flycheck-ledger))

;; less-css
(use-package less-css-mode
  :mode "\\.less\\'")

;; magit
(use-package magit
  :commands (magit-status projectile-vc)
  :config
  (progn
    (add-to-list 'magit-log-arguments "--no-abbrev-commit")
    (setq magit-push-always-verify nil
	  magit-popup-use-prefix-argument 'default
	  magit-revert-buffers t)))

(global-git-commit-mode)

;; markdown
(use-package markdown-mode
  :mode ("\\.markdown\\'" "\\.mk?d\\'" "\\.text\\'"))

;; matlab
(use-package matlab-mode
  :mode "\\.m$")

;; multi-term
(use-package multi-term
  :bind ("C-c t" . multi-term-dedicated-toggle)
  :commands (multi-term)
  :config
  (progn
    (setq multi-term-program "bash"
	  multi-term-dedicated-close-back-to-open-buffer-p t)
    (add-to-list 'term-bind-key-alist '("M-DEL" . term-send-backward-kill-word))
    (add-to-list 'term-bind-key-alist '("M-d" . term-send-forward-kill-word))))

;; multiple-cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; nginx
(use-package nginx-mode
  :mode ("nginx.conf$" "/etc/nginx/.*"))

;; org mode extensions
(use-package org-plus-contrib
  :mode (("\\.org\\'" . org-mode) ("[0-9]\\{8\\}\\'" . org-mode))
  :init
  (progn
    (use-package org-journal
      :bind ("C-c j" . org-journal-new-entry))
    (use-package org-pomodoro
      :commands (org-pomodoro))
    (add-hook 'org-mode-hook 'turn-on-auto-fill)
    (setq org-latex-listings t
	  org-pretty-entities t
          org-completion-use-ido t
	  org-latex-custom-lang-environments '((C "lstlisting"))
          org-entities-user '(("join" "\\Join" nil "&#9285;" "" "" "⋈")
                              ("reals" "\\mathbb{R}" t "&#8477;" "" "" "ℝ")
                              ("ints" "\\mathbb{Z}" t "&#8484;" "" "" "ℤ")
                              ("complex" "\\mathbb{C}" t "&#2102;" "" "" "ℂ")
                              ("models" "\\models" nil "&#8872;" "" "" "⊧"))
          org-export-backends '(html beamer ascii latex md))
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t) (gnuplot . t) (C . t) (emacs-lisp . t) (haskell . t)
       (latex . t) (ledger . t) (python . t) (ruby . t) (sh . t)))))

;; code folding
(use-package origami
  :bind* ("C-S-o" . origami-mode)
  :config
  (progn
    (add-to-list 'origami-parser-alist '(processing-mode . origami-c-style-parser))
    (bind-keys :prefix-map origami-prefix-map
	       :prefix "C-o"
	       ("o" . origami-recursively-toggle-node)
	       ("a" . origami-toggle-all-nodes)
	       ("c" . origami-show-only-node))))

;; popwin
(use-package popwin
  :config
  (progn
    (popwin-mode)
    ;; cannot use :bind for keymap
    (global-set-key (kbd "C-z") popwin:keymap)))

;; powershell
(use-package powershell)

;; processing
(use-package processing-mode
  :mode "\\.pde$"
  :config (use-package processing-snippets))

;; fix https://github.com/bbatsov/projectile/issues/837
(setq grep-find-ignored-files nil
      grep-find-ignored-directories nil)

;; projectile
(use-package projectile
  :diminish projectile-mode
  :bind* ("M-[" . projectile-command-map)
  :demand
  :config
  (progn
    (setq projectile-completion-system 'helm
	  projectile-switch-project-action 'helm-projectile
	  projectile-enable-caching t
	  projectile-file-exists-remote-cache-expire (* 10 60))
    (use-package helm-projectile
      :commands helm-projectile
      :config (helm-projectile-on))
    (projectile-global-mode)))

;; puppet
(use-package puppet-mode
  :mode "\\.pp\\'")

;; regex tool
(use-package regex-tool
  :commands (regex-tool))

;; ruby
(use-package ruby-mode)

;; rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :config (use-package flycheck-rust))

;; save kill ring
(use-package savekill)

;; saveplace
(use-package saveplace
  :config
  (setq-default save-place t
                save-place-file (f-expand "saved-places" user-emacs-directory)))
;; scratch
(use-package scratch
  :commands (scratch))

;; use a Lisp commented fortune for the initial scratch message
(when (executable-find "fortune")
  (setq initial-scratch-message
	(concat
	 (mapconcat
	  (lambda (x) (concat ";; " x))
	  (split-string (shell-command-to-string "fortune") "\n" t) "\n")
	 "\n\n")))

;; slime
(use-package sly
  :commands (sly)
  :config (setq inferior-lisp-program (executable-find "sbcl")))

;; smart-mode-line
(use-package smart-mode-line
  :config
  (progn
    (setq sml/theme nil
	  sml/shorten-directory t
	  sml/name-width '(32 . 48)
	  sml/shorten-modes t
	  sml/use-projectile-p 'before-prefixes
	  sml/projectile-replacement-format "[%s]")
    (sml/setup)))

;; smart tabs
(use-package smart-tabs-mode
  :disabled t
  :config (smart-tabs-insinuate 'c 'c++ 'python 'ruby))

;; activate smartparens
(use-package smartparens
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode)
    (show-smartparens-global-mode)))

;; ssh-config
(use-package ssh-config-mode
  :mode ((".ssh/config\\'"       . ssh-config-mode)
	 ("sshd?_config\\'"      . ssh-config-mode)
	 ("known_hosts\\'"       . ssh-known-hosts-mode)
	 ("authorized_keys2?\\'" . ssh-authorized-keys-mode)))

;; toml
(use-package toml-mode
  :mode "\\.toml$")

;; tramp
(use-package tramp
  :config
  (progn
    (setq tramp-verbose 9
	  tramp-default-method "ssh"
	  tramp-ssh-controlmaster-options
	  (concat "-o ControlPath=/tmp/tramp.%%r@%%h:%%p "
		  "-o ControlMaster=auto "
		  "-o ControlPersist=no"))))

;; try
(use-package try
  :commands try)

;; undo-tree
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (add-to-list 'undo-tree-history-directory-alist
                 `("." . ,(f-expand "undo-tree" user-emacs-directory)))
    (setq undo-tree-auto-save-history t)))

;; unfill autofill
(use-package unfill
  :commands (unfill-region unfill-paragraph toggle-fill-unfill))

;; uniquify
(use-package uniquify
  :ensure nil
  :config (setq uniquify-buffer-name-style 'forward))

;; setup virtualenvwrapper
(use-package virtualenvwrapper
  :commands (venv-workon))

;; way better regexp
(use-package visual-regexp
  :bind ("M-%" . vr/query-replace))

;; which-key
(use-package which-key
  :config (which-key-mode))

;; whitespace
(use-package whitespace
  :commands (whitespace-mode)
  :config
  (setq whitespace-style '(face tabs spaces newline empty
                                trailing tab-mark newline-mark)))

(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :commands (global-whitespace-cleanup-mode))

(use-package ws-butler
  :diminish ws-butler-mode
  :init (ws-butler-global-mode))

;; yaml
(use-package yaml-mode
  :mode "\\.ya?ml\'")

;; yasnippet
(use-package yasnippet
  :commands (yas-expand yas-insert-snippet)
  :config
  (progn
    (use-package java-snippets)
    (yas-minor-mode)))

;; znc
(use-package znc
  :if (bound-and-true-p znc-password)
  :commands znc-erc
  :config (setq znc-servers
		`(("schwartzmeyer.com" .
		   (46728 t ((freenode . ("andrew/freenode" ,znc-password))))))))

;;; start server
(server-start)

;;; provide init package
(provide 'init)

;;; init.el ends here
