;;; init --- Andrew Schwartzmeyer's Emacs init file

;;; Commentary:
;;; Fully customized Emacs configurations

;;; Code:

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(setq use-package-verbose t)
(require 'use-package)

(use-package diminish)

;;; shortcuts

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
(bind-key* "M-s" 'ace-window)
(bind-key* "M-1" 'delete-other-windows)
(bind-key* "M-2" 'split-window-vertically)
(bind-key* "M-3" 'split-window-horizontally)
(bind-key* "M-0" 'delete-window)

;;; appearance

;; theme (zenburn in terminal, Solarized otherwise)
(use-package solarized
  :if (display-graphic-p)
  :config
  (progn
    ;; disable toolbar and scrollbar
    (tool-bar-mode 0)
    (scroll-bar-mode 0)
    (setq solarized-use-variable-pitch nil
	  solarized-scale-org-headlines nil)
    (load-theme 'solarized-dark t)))

(use-package zenburn-theme
  :if (not (display-graphic-p))
  :init (load-theme 'zenburn))

(setq enable-recursive-minibuffers t)

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

;; kill whole line (including newline)
(setq kill-whole-line t)

;; initial text mode
(setq initial-major-mode 'text-mode)

;; default truncate lines
(set-default 'truncate-lines t)

;; longer commit summaries
(setq git-commit-summary-max-length 72)

;; disable bell
(setq ring-bell-function 'ignore)

;; subword navigation
(global-subword-mode)

;; increase garbage collection threshold
(setq gc-cons-threshold 20000000)

;; inhibit startup message
(setq inhibit-startup-message t)

;; remove selected region if typing
(pending-delete-mode t)

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
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))
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

;; select whole line
(defun select-whole-line ()
  "Select whole line which has the cursor."
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))
(bind-key "C-c w" 'select-whole-line)

;; comment/uncomment line/region
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))
(bind-key "C-c c" 'comment-or-uncomment-region-or-line)

;;; load local settings
(use-package local
  :load-path "site-lisp/")

;; load OS X configurations
(use-package osx
  :if (eq system-type 'darwin)
  :load-path "lisp/")

;;; packages

;; ace-jump-mode
(use-package ace-jump-mode
  :bind (("C-." . ace-jump-mode)
         ("C-," . ace-jump-mode-pop-mark))
  :config (eval-after-load "ace-jump-mode" '(ace-jump-mode-enable-mark-sync)))

;; ace-window
(use-package ace-window
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; ag - the silver searcher
(use-package ag
  :commands (ag ag-files ag-regexp ag-project ag-dired helm-ag)
  :bind ("C-c s" . helm-ag)
  :config (setq ag-highlight-search t
		ag-reuse-buffers t))

;; Mercurial
(use-package ahg)

;; anzu - number of search matches in modeline
(use-package anzu
  :init
  (progn
    (global-anzu-mode)
    (diminish 'anzu-mode)))

;; bison
(use-package bison-mode
  :mode (("\\.y\\'" . bison-mode)
         ("\\.l\\'" . bison-mode)))

;; company "complete anything"
(use-package company
  :commands (helm-company)
  :bind ("<backtab>" . helm-company)
  :config
  (progn
    (company-mode)
    (use-package helm-company
      :config
      (progn
	(define-key company-mode-map (kbd "C-:") 'helm-company)
	(define-key company-active-map (kbd "C-:") 'helm-company)))
    (push '(company-clang
	    :with company-semantic
	    :with company-yasnippet
	    :with company-c-headers)
          company-backends)
    (setq company-minimum-prefix-length 2
          company-idle-delay nil
	  company-global-modes '(not gud-mode))))

;; crontab
(use-package crontab-mode
  :mode ("\\.cron\\(tab\\)?\\'" . crontab-mode))

;; activate expand-region
(use-package expand-region
  :bind ("C-=" . er/expand-region))


;; flycheck
(use-package flycheck
  :bind (("C-c ! c" . flycheck-buffer)
	 ("C-c ! h" . helm-flycheck))
  :config (flycheck-mode))

;; flyspell - use aspell instead of ispell
(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :config (setq ispell-program-name (executable-find "aspell")
                ispell-extra-args '("--sug-mode=ultra")))

;; gnuplot
(use-package gnuplot
  :commands (gnuplot-mode gnuplot-make-buffer))

;; handlebars
(use-package handlebars-mode
  :mode (("\\.handlebars\'" . handlebars-mode)
         ("\\.hbs\'" . handlebars-mode)))

;; haskell
(use-package haskell-mode
  :mode ("\\.\\(?:[gh]s\\|hi\\)\\'" . haskell-mode)
  :interpreter (("runghc" . haskell-mode)
                ("runhaskell" . haskell-mode)))

;; helm
(use-package helm-config
  :bind* (("M-x" . helm-M-x)
	  ("M-]" . helm-command-prefix)
	  ("M-y" . helm-show-kill-ring)
	  ("C-x C-b" . helm-buffers-list)
	  ("C-x b" . helm-mini)
	  ("C-x C-f" . helm-find-files))
  :init
  (progn
    (helm-mode)
    (helm-autoresize-mode t)
    (setq helm-M-x-fuzzy-match t
	  helm-recentf-fuzzy-match t
	  helm-buffers-fuzzy-matching t
	  helm-semantic-fuzzy-match t
	  helm-imenu-fuzzy-match t
	  helm-apropos-fuzzy-match t
	  helm-move-to-line-cycle-in-source t
	  helm-split-window-in-side-p t
	  helm-ff-file-name-history-use-recentf t
	  helm-ff-auto-update-initial-value nil)
    (diminish 'helm-mode)))

;; (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
;; (bind-key "C-i" 'helm-execute-persistent-action helm-map)
;; (bind-key "C-z" 'helm-select-action helm-map)

;; ledger
(use-package ledger-mode
  :mode ("\\.ledger\\'" . ledger-mode))

;; magit
(use-package magit
  :config
  (progn
    (setq magit-log-arguments '("--graph" "--decorate" "--show-signature"))
    (magit-define-popup-option 'magit-patch-popup
      ?S "Subject Prefix" "--subject-prefix=")
    (magit-define-popup-option 'magit-merge-popup
      ?X "Strategy Option" "--strategy-option=")
    (magit-auto-revert-mode)
    (diminish 'magit-auto-revert-mode)))

;; markdown
(use-package markdown-mode
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.mk?d\\'" . markdown-mode)))

;; multi-term
(use-package multi-term
  :commands (multi-term)
  :config
  (progn
    (setq multi-term-program "zsh"
	  term-buffer-maximum-size 10000)
    (add-to-list 'term-bind-key-alist '("M-DEL" . term-send-backward-kill-word))
    (add-to-list 'term-bind-key-alist '("M-d" . term-send-forward-kill-word))))

;; multiple-cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; org mode extensions
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (progn
    ;; pomodoro
    (use-package org-pomodoro
      :commands (org-pomodoro))
    (add-hook 'org-mode-hook 'turn-on-auto-fill)
    (setq org-latex-listings t
	  org-pretty-entities t
          org-completion-use-ido t
          org-entities-user '(("join" "\\Join" nil "&#9285;" "" "" "⋈")
                              ("reals" "\\mathbb{R}" t "&#8477;" "" "" "ℝ")
                              ("ints" "\\mathbb{Z}" t "&#8484;" "" "" "ℤ")
                              ("complex" "\\mathbb{C}" t "&#2102;" "" "" "ℂ")
                              ("models" "\\models" nil "&#8872;" "" "" "⊧"))
          org-export-backends '(html beamer ascii latex md))
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (gnuplot . t)
       (C . t)
       (emacs-lisp . t)
       (haskell . t)
       (latex . t)
       (ledger . t)
       (python . t)
       (ruby . t)
       (sh . t)))))

;; popwin
(use-package popwin
  :config
  (progn
    (popwin-mode)
    ;; cannot use :bind for keymap
    (global-set-key (kbd "C-z") popwin:keymap)))

;; projectile
(use-package projectile
  ;; projectile command map
  :bind* ("M-[" . projectile-command-map)
  :init (projectile-global-mode)
  :config
  (progn
    (setq projectile-completion-system 'helm
	  projectile-switch-project-action 'helm-projectile
	  projectile-enable-caching t
	  projectile-file-exists-remote-cache-expire (* 10 60))
    (helm-projectile-on)
    (diminish 'projectile-mode)))

;; puppet
(use-package puppet-mode
  :mode ("\\.pp\\'" . puppet-mode))

;; regex tool
(use-package regex-tool
  :commands (regex-tool))

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

;; slime
(use-package sly
  :commands (sly)
  :config (setq inferior-lisp-program (executable-find "sbcl")))

;; smart-mode-line
(use-package smart-mode-line
  :init
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
  :config (smart-tabs-insinuate 'c 'c++ 'python 'ruby))

;; activate smartparens
(use-package smartparens
  :config
  (progn
    (use-package smartparens-config)
    (smartparens-global-mode)
    (show-smartparens-global-mode)
    (diminish 'smartparens-mode)))

;; scrolling
(use-package smooth-scroll
  :if (display-graphic-p)
  :init
  (progn
    (setq smooth-scroll/vscroll-step-size 8)
    (smooth-scroll-mode)
    (diminish 'smooth-scroll-mode)))

;; undo-tree
(use-package undo-tree
  :config
  (progn
    (diminish 'undo-tree-mode)
    (global-undo-tree-mode)
    (add-to-list 'undo-tree-history-directory-alist
                 `("." . ,(f-expand "undo-tree" user-emacs-directory)))
    (setq undo-tree-auto-save-history t)))

;; uniquify
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

;; setup virtualenvwrapper
(use-package virtualenvwrapper
  :commands (venv-workon))

;; whitespace
(use-package whitespace
  :commands (whitespace-mode)
  :config
  (setq whitespace-style '(face tabs spaces newline empty
                                trailing tab-mark newline-mark)))

(use-package whitespace-cleanup-mode
  :config (global-whitespace-cleanup-mode))

;; yaml
(use-package yaml-mode
  :mode (("\\.ya?ml\'" . yaml-mode)))

;; yasnippet
(use-package yasnippet
  :commands (yas-expand yas-insert-snippet)
  :config (yas-minor-mode))

;;; start server
(server-start)

;;; provide init package
(provide 'init)

;;; init.el ends here
