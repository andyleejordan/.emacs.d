;;; init --- Andrew Schwartzmeyer's Emacs init file

;;; Commentary:
;;; Fully customized Emacs configurations

;;; Code:

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'use-package)

;;; shortcuts
;; miscellaneous
(bind-key "M-/" 'hippie-expand)
(bind-key "C-c a" 'org-agenda)
(bind-key "C-c x" 'eval-buffer)
;; isearch
(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "C-r" 'isearch-backward-regexp)
(bind-key "C-M-s" 'isearch-forward)
(bind-key "C-M-r" 'isearch-backward)

;;; appearance
;; theme (wombat in terminal, solarized otherwise)
(if (display-graphic-p)
    (use-package solarized
      :init
      (progn
	(setq solarized-use-variable-pitch nil)
	(setq solarized-high-contrast-mode-line t)
	(load-theme 'solarized-dark t)))
  (load-theme 'wombat t))
;; font size
(set-face-attribute 'default nil :height 120)
;; line/column numbers in mode-line
(setq line-number-mode t)
(setq column-number-mode t)
;; y/n for yes/no
(defalias 'yes-or-no-p 'y-or-n-p)
;; quit prompt
(setq confirm-kill-emacs 'yes-or-no-p)
;; start week on Monday
(setq calendar-week-start-day 1)
;; cursor settings
(blink-cursor-mode t)
;; visually wrap lines
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;; default truncate lines
(setq-default truncate-lines t)
;; matching parentheses
(show-paren-mode t)
;; window undo/redo
(winner-mode t)

;;; settings
;; pull in path from shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
;; enable all commands
(setq disabled-command-function nil)
;; kill whole line (including newline)
(setq kill-whole-line t)
;; initial text mode
(setq initial-major-mode 'text-mode)
;; enable auto-fill mode for text
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; disable auto-fill because of stupid GitHub flavored markdown
(remove-hook 'git-commit-mode-hook 'turn-on-auto-fill)
(add-hook 'git-commit-mode-hook 'turn-on-visual-line-mode)
;; disable bell
(setq ring-bell-function 'ignore)
;; subword navigation
(global-subword-mode t)
;; increase garbage collection threshold
(setq gc-cons-threshold 20000000)
;; inhibit startup message
(setq inhibit-startup-message t)
;; remove selected region if typing
(pending-delete-mode 1)
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
  '(progn (setenv "TMPDIR" "/tmp")))

;;; files
;; auto-save
(setq auto-save-timeout 60)
;; backups
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(setq backup-directory-alist `(("." . ,(concat
					user-emacs-directory "backups"))))
;; final-newline
(setq require-final-newline 't)
;; set auto revert of buffers if file is changed externally
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(global-auto-revert-mode t)
;; symlink version-control follow
(setq vc-follow-symlinks t)
;; set arduino *.ino files to c-mode
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))
;; set Emacs Lisp files mode
(add-to-list 'magic-mode-alist '(";;; " . emacs-lisp-mode))
;;; functions
;; load local file
(defun load-local (file)
  "Load FILE from ~/.emacs.d."
  (load (f-expand file user-emacs-directory)))
;; select whole line
(defun select-whole-line ()
  "Select whole line which has the cursor."
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))
(bind-key "C-c l" 'select-whole-line)
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
(load-local "feeds")
(load-local "private")

;;; load OS X configurations
(when (eq system-type 'darwin)
  (load-local "osx"))

;;; packages
;; ace-jump-mode
(use-package ace-jump-mode
  :config (eval-after-load "ace-jump-mode" '(ace-jump-mode-enable-mark-sync))
  :bind (("C-c ," . ace-jump-mode)
	 ("C-x ," . ace-jump-mode-pop-mark)))
;; browse-kill-ring
(use-package browse-kill-ring
  :config (browse-kill-ring-default-keybindings)
  :bind ("C-c k" . browse-kill-ring))
;; company "complete anything"
(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode))
;; ein
(use-package ein
  :config (setq ein:use-auto-complete t))
;; elfeed
(use-package elfeed
  :bind ("C-x w" . elfeed))
;; erc --- configured with help from:
;; http://emacs-fu.blogspot.com/2009/06/erc-emacs-irc-client.html
(use-package erc
  :init
  (progn
    (defun erc-start-or-switch ()
      "Connect to ERC, or switch to last active buffer"
      (interactive)
      (if (get-buffer "chat.freenode.net:7000") ;; ERC already active?
	  (erc-track-switch-buffer 1) ;; yes: switch to last active
	(when (yes-or-no-p "Start ERC? ") ;; no: maybe start ERC
	  (erc-tls :server "chat.freenode.net" :port
		   7000 :nick "andschwa" :full-name "Andrew Schwartzmeyer")))))
  :config
  (progn
    (use-package erc-services
      :config (erc-services-mode t))
    (use-package erc-notify
      :config
      (progn
	(erc-notify-mode t)
	(setq erc-notify-list '("p_nathan1"))))
    (erc-spelling-mode t) ;; flyspell
    (erc-track-mode t)
    (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				    "324" "329" "332" "333" "353" "477"))
    ;; don't show any of this
    (setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
    ;; nicks
    (setq erc-prompt-for-nickserv-password nil)
    (setq erc-nickserv-passwords
	  `((freenode (("andschwa" . ,irc-freenode-andschwa-pass)))))
    ;; channel autojoin
    (erc-autojoin-mode nil)
    (setq erc-autojoin-timing 'ident))
  :bind ("C-c e" . erc-start-or-switch))
;; activate expand-region
(use-package expand-region
  :bind ("C-=" . er/expand-region))
;; flx-ido
(use-package flx-ido
  :init
  (progn
    (flx-ido-mode t)
    (setq ido-use-faces nil)))
;; flycheck
(use-package flycheck
  :init
  (progn
    (add-hook 'after-init-hook #'global-flycheck-mode)
    (add-hook 'c++-mode-hook
	      (lambda ()
		(setq-default flycheck-clang-standard-library "libc++")
		(setq-default flycheck-clang-language-standard "gnu++11")))))
;;; flyspell
(use-package flyspell
  :config (setq ispell-program-name "aspell" ; use aspell instead of ispell
	      ispell-extra-args '("--sug-mode=ultra")))
;; ibuffer
(use-package ibuffer
  :config (add-hook 'ibuffer-mode-hook (lambda () (setq truncate-lines t)))
  :bind ("C-x C-b" . ibuffer))
;; ido setup
(use-package ido
  :init (ido-mode t))
(use-package ido-ubiquitous)
;; ido-vertical
(use-package ido-vertical-mode
  :init (ido-vertical-mode t))
;; multiple-cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)))
;; multi-term
(use-package multi-term
  :config (setq multi-term-program "bash"))
;; notmuch
(use-package notmuch
  :config (setq notmuch-command "~/bin/notmuch")
  :bind ("C-x n" . notmuch))
;;; org-mode
;; org-agenda
(setq org-agenda-files '("~/.org"))
;; org-auto-fill
(add-hook 'org-mode-hook 'turn-on-auto-fill)
;; org-entities
(setq org-pretty-entities t)
(setq org-entities-user '(("join" "\\Join" nil "&#9285;" "" "" "⋈")
			  ("reals" "\\mathbb{R}" t "&#8477;" "" "" "ℝ")
			  ("ints" "\\mathbb{Z}" t "&#8484;" "" "" "ℤ")
			  ("complex" "\\mathbb{C}" t "&#2102;" "" "" "ℂ")
			  ("models" "\\models" nil "&#8872;" "" "" "⊧")))
;; org-export
(setq org-export-backends '(html beamer ascii latex md))
;; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (gnuplot . t)
   (C . t)
   (latex . t)
   (python . t)
   (ruby . t)
   (sh . t)))
;; popwin
(use-package popwin
  :init
  (progn
    (popwin-mode 1)
    ;; cannot use :bind for keymap
    (global-set-key (kbd "C-z") popwin:keymap)))
;; activate projectile
(use-package projectile
  :config (projectile-global-mode))
;;; save kill ring
(use-package savekill)
;;; saveplace
(use-package saveplace
  :init
  (progn
    (setq-default save-place t)
    (setq save-place-file (concat user-emacs-directory "saved-places"))))
;; scratch
(use-package scratch
  :bind ("C-c s" . scratch))
;; activate smartparens
(use-package smartparens
  :init (progn (smartparens-global-mode t)
	       (show-smartparens-global-mode t)
	       (use-package smartparens-config)))
;; setup smex bindings
(use-package smex
  :init
  (progn
    (setq smex-save-file (expand-file-name ".smex-items" "~/.emacs.d/"))
    (smex-initialize))
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ("C-c C-c M-x" . execute-extended-command)))
;; scrolling
(use-package smooth-scroll
  :init
  (progn
    (smooth-scroll-mode t)
    (setq smooth-scroll/vscroll-step-size 8)))
;; undo-tree
(use-package undo-tree
  :init (global-undo-tree-mode t))
;;; uniquify
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))
;; setup virtualenvwrapper
(use-package virtualenvwrapper
  :config (setq venv-location "~/.virtualenvs/"))
;;; whitespace
(use-package whitespace
  :init (add-hook 'before-save-hook 'whitespace-cleanup)
  :config
  (progn
    (setq whitespace-line-column 80) ;; limit line length
    (setq whitespace-style '(face tabs empty trailing lines-tail))))
;;; yasnippet
(use-package yasnippet)

;;; start server
(server-start)

;;; provide init package
(provide 'init)

;;; init.el ends here
