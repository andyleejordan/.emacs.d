;;; init --- Andrew Schwartzmeyer's Emacs init file

;;; Commentary:
;;; Fully customized Emacs configurations

;;; Code:

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(setq use-package-verbose t)
(require 'use-package)

;;; shortcuts

;; miscellaneous
(bind-key "C-c l" 'align-regexp)
(bind-key "C-c x" 'eval-buffer)
(bind-key "C-c q" 'auto-fill-mode)

;; isearch
(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "C-r" 'isearch-backward-regexp)
(bind-key "C-M-s" 'isearch-forward)
(bind-key "C-M-r" 'isearch-backward)

;; window management
(bind-key* "M-3" 'delete-other-windows)
(bind-key* "M-4" 'split-window-horizontally)
(bind-key* "M-5" 'split-window-vertically)
(bind-key* "M-2" 'delete-window)
(bind-key* "M-s" 'other-window)

;;; appearance

;; theme (wombat in terminal, solarized otherwise)
(use-package solarized
  :if (display-graphic-p)
  :config
  (progn
    (setq solarized-use-variable-pitch nil)
    (load-theme 'solarized-dark t)))

(use-package wombat
  :if (not (display-graphic-p))
  :init (load-theme 'tango))

;; scrolling
(use-package smooth-scroll
  :if (display-graphic-p)
  :idle (smooth-scroll-mode)
  :config (setq smooth-scroll/vscroll-step-size 8))

;; line/column numbers in mode-line
(line-number-mode)
(column-number-mode)

;; y/n for yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; quit prompt
(setq confirm-kill-emacs 'yes-or-no-p)

;; start week on Monday
(setq calendar-week-start-day 1)

;; cursor settings
(blink-cursor-mode)

;; visually wrap lines
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; default truncate lines
(setq-default truncate-lines t)

;; matching parentheses
(show-paren-mode)

;; window undo/redo
(winner-mode)

;; show function in modeline
(which-function-mode)

;;; settings

;; enable all commands
(setq disabled-command-function nil)

;; kill whole line (including newline)
(setq kill-whole-line t)

;; initial text mode
(setq initial-major-mode 'text-mode)

;; visual line mode for text
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; longer commit summaries
(setq git-commit-summary-max-length 72)

;; disable bell
(setq ring-bell-function 'ignore)

;; subword navigation
(subword-mode)

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

;; open empty files quietly
(setq confirm-nonexistent-file-or-buffer nil)

;; fix tramp
(eval-after-load 'tramp
  '(progn (setenv "TMPDIR" "/tmp")
	  (setenv "SHELL" "/bin/sh")))
(setq tramp-auto-save-directory "/tmp")

;;; files

;; backups
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 4
      kept-old-versions 2
      version-control t
      vc-make-backup-files t
      backup-directory-alist `(("." . ,(concat
					user-emacs-directory "backups"))))

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
(setq
 ; enable side-by-side dired buffer targets
 dired-dwim-target t
 ; better recursion in dired
 dired-recursive-copies 'always
 dired-recursive-deletes 'top)

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
  "Comments or uncomments the region or the current line if there's no active region."
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

;; ag - the silver searcher
(use-package ag
  :commands (ag)
  :bind ("C-c s" . ag))

;; anzu - number of search matches in modeline
(use-package anzu
  :idle (global-anzu-mode))

;; bison
(use-package bison-mode
  :mode (("\\.y\\'" . bison-mode)
	 ("\\.l\\'" . bison-mode)))

;; browse-kill-ring
(use-package browse-kill-ring
  :bind ("C-c k" . browse-kill-ring)
  :config (browse-kill-ring-default-keybindings))

;; company "complete anything"
(use-package company
  :bind ("C-c <tab>" . company-complete)
  :config
  (progn
    (global-company-mode)
    (setq company-minimum-prefix-length 2
	  company-idle-delay 0.1)))

;; crontab
(use-package crontab-mode
  :mode ("\\.cron\\(tab\\)?\\'" . crontab-mode))

;; ein
(use-package ein
  :commands (ein:notebooklist-open)
  :config (setq ein:use-auto-complete t))

;; elfeed
(use-package elfeed
  :bind ("C-x w" . elfeed)
  :config (add-hook 'elfeed-new-entry-hook
		    (elfeed-make-tagger :before "2 weeks ago"
					:remove 'unread)))

;; activate expand-region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; flycheck
(use-package flycheck
  :bind ("C-c ! c" . flycheck-buffer)
  :config
  (progn
    (global-flycheck-mode)
    (setq flycheck-completion-system 'ido)
    (use-package "flycheck-ledger")))

;; flyspell
(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :config (setq ispell-program-name (executable-find "aspell") ; use aspell instead of ispell
		ispell-extra-args '("--sug-mode=ultra")))

;; fullframe
(use-package fullframe
  :config (fullframe magit-status magit-mode-quit-window nil))

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

;; ibuffer
(use-package ibuffer
  :config (add-hook 'ibuffer-mode-hook (lambda () (setq truncate-lines t)))
  :bind ("C-x C-b" . ibuffer))

;; ido setup
(use-package ido
  :config
  (progn
    (ido-mode)
    (use-package ido-ubiquitous
      :config (ido-ubiquitous-mode))
    (use-package flx-ido
      :config
      (progn
	(flx-ido-mode)
	(setq ido-enable-flex-matching t
	      ido-use-faces nil)))
    (use-package ido-vertical-mode
      :config (ido-vertical-mode))))

;; ledger
(use-package ledger-mode
  :mode ("\\.ledger\\'" . ledger-mode))

;; magit
(use-package magit
  :commands (magit-status)
  :config (setq magit-completing-read-function 'magit-ido-completing-read))

;; markdown
(use-package markdown-mode
  :mode (("\\.markdown\\'" . markdown-mode)
	 ("\\.mk?d\\'" . markdown-mode)))

;; multi-term
(use-package multi-term
  :commands (multi-term)
  :config (setq multi-term-program "bash"))

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
    (setq org-pretty-entities t
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
  :init (projectile-global-mode))

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
		save-place-file (f-expand "saved-places" user-emacs-directory )))
;; scratch
(use-package scratch
  :commands (scratch))

;; slime
(use-package sly
  :commands (sly)
  :config (setq inferior-lisp-program (executable-find "sbcl")))

;; smart-mode-line
(use-package smart-mode-line
  :config
  (progn
    (sml/setup)
    (sml/apply-theme 'automatic)))

;; smart tabs
(use-package smart-tabs-mode
  :config (smart-tabs-insinuate 'c 'c++ 'python 'ruby))

;; activate smartparens
(use-package smartparens
  :config
  (progn (use-package smartparens-config)
	 (smartparens-global-mode)
	 (show-smartparens-global-mode)))

;; setup smex bindings
(use-package smex
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ("C-c C-c M-x" . execute-extended-command))
  :config
  (progn
    (setq smex-save-file (f-expand "smex-items" user-emacs-directory))
    (smex-initialize)))

;; undo-tree
(use-package undo-tree
  :config
  (progn
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

(use-package w3m
  :commands (w3m w3m-browse-url)
  :config (setq w3m-command (executable-find "w3m")))

;; whitespace
(use-package whitespace
  :init (global-whitespace-mode)
  :commands (whitespace-mode)
  :config
  (setq whitespace-global-modes '(not org-mode)
	whitespace-line-column 80 ;; limit line length
	whitespace-style '(face tabs empty trailing lines-tail)
	whitespace-action '(auto-cleanup)))

;; yaml
(use-package yaml-mode
  :mode (("\\.ya?ml\'" . yaml-mode)))

;; yasnippet
(use-package yasnippet
  :idle (yas-global-mode)
  :config
  (progn
    (unbind-key "<tab>" yas-minor-mode-map)
    (unbind-key "TAB" yas-minor-mode-map)
    (bind-key "C-c y" 'yas-expand yas-minor-mode-map)))

;;; start server
(server-start)

;;; provide init package
(provide 'init)

;;; init.el ends here
