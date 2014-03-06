;;; init --- Andrew Schwartzmeyer's Emacs init file

;;; Commentary:
;;; Fully customized Emacs configurations

;;; Code:

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'use-package)

;;; shortcuts
;; miscellaneous
(define-key global-map (kbd "C-x c") 'compile)
(define-key global-map (kbd "C-x g") 'magit-status)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c d") 'dash-at-point)
(define-key global-map (kbd "C-c x") 'eval-buffer)
;; isearch
(define-key global-map (kbd "C-s") 'isearch-forward-regexp)
(define-key global-map (kbd "C-r") 'isearch-backward-regexp)
(define-key global-map (kbd "C-M-s") 'isearch-forward)
(define-key global-map (kbd "C-M-r") 'isearch-backward)

;;; appearance
;; font size
(set-face-attribute 'default nil :height 120)
;; solarized-dark theme
(setq solarized-use-variable-pitch nil)
(setq solarized-high-contrast-mode-line t)
(load-theme 'solarized-dark t)
;; disable toolbar and scrollbar
(tool-bar-mode 0)
(scroll-bar-mode 0)
;; column-number-mode
(setq column-number-mode t)
;; y/n for yes/no
(defalias 'yes-or-no-p 'y-or-n-p)
;; quit prompt
(setq confirm-kill-emacs 'yes-or-no-p)
;; start week on Monday
(setq calendar-week-start-day 1)
;; blink cursor
(blink-cursor-mode t)
;; visually wrap lines
(global-visual-line-mode t)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;; matching parentheses
(show-paren-mode t)

;;; settings
;; enable all commands
(setq disabled-command-function nil)
;; kill whole line (including newline)
(setq kill-whole-line t)
;; initial text mode
(setq initial-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
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
;; open empty files quietly
(setq confirm-nonexistent-file-or-buffer nil)
;; fix tramp
(eval-after-load 'tramp '(setenv "SHELL" "/bin/sh"))

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
(define-key global-map (kbd "C-c l") 'select-whole-line)
;; comment/uncomment line/region
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))
(define-key global-map (kbd "C-c c") 'comment-or-uncomment-region-or-line)

;;; erc --- configured with help from:
;; http://emacs-fu.blogspot.com/2009/06/erc-emacs-irc-client.html
(use-package erc
  :init
  (progn
    (defun erc-start-or-switch ()
      "Connect to ERC, or switch to last active buffer"
      (interactive)
      (if (get-buffer "chat.freenode.net:7000") ;; ERC already active?
	  (erc-track-switch-buffer 1) ;; yes: switch to last active
	(when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
	  (erc-tls :server "chat.freenode.net" :port
		   7000 :nick "andschwa" :full-name "Andrew Schwartzmeyer"))))
    (define-key global-map (kbd "C-c e") 'erc-start-or-switch))
  :config
  (progn
    (load "~/.ercpass")
    (use-package tls)
    (use-package tls)
    (use-package erc-services
      :init
      (erc-services-mode t))
    (use-package erc-notify
      :init
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
    (setq erc-autojoin-timing 'ident)))

;;; org-mode
;; org-journal
(use-package org-journal)
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

;;; non use-package
;; scratch
(autoload 'scratch "scratch" nil t)
;; pull in shell path
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
;; flycheck
(setq-default flycheck-clang-standard-library "libc++")
(setq-default flycheck-clang-language-standard "c++11")
(add-hook 'after-init-hook #'global-flycheck-mode)
;; undo-tree
(global-undo-tree-mode t)
;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(add-hook 'ibuffer-mode-hook (lambda () (setq truncate-lines t)))

;;; packages
;; ace-jump-mode
(use-package ace-jump-mode
  :init
  (progn
    (autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
    (autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back:-)" t)
    (eval-after-load "ace-jump-mode"
      '(ace-jump-mode-enable-mark-sync)))
  :bind (("C-c SPC" . ace-jump-mode)
	 ("C-x SPC" . ace-jump-mode-pop-mark)))
;; auto-complete
(use-package auto-complete
  :init (global-auto-complete-mode t)
  :bind ("M-/" . hippie-expand))
;; drag-stuff
(use-package drag-stuff
  :init (drag-stuff-global-mode t))
;; activate expand-region
(use-package expand-region
  :bind ("C-=" . er/expand-region))
;; browse-kill-ring
(use-package browse-kill-ring
  :init (browse-kill-ring-default-keybindings)
  :bind ("C-c k" . browse-kill-ring))
;; ein
(use-package ein
  :init (setq ein:use-auto-complete t))
;; require ido-ubiquitous
(use-package ido
  :init (ido-mode t))
(use-package ido-ubiquitous)
;; ido-vertical
(use-package ido-vertical-mode
  :init (ido-vertical-mode t))
;; flx-ido
(use-package flx-ido
  :init
  (progn
    (flx-ido-mode t)
    (setq ido-use-faces nil)))
;; activate projectile
(use-package projectile
  :init (projectile-global-mode))
;; move-text
(use-package move-text
  :init (move-text-default-bindings))
;; multiple-cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)))
;; popwin
(use-package popwin
  :init (popwin-mode 1))
;; activate smartparens
(use-package smartparens-config)
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
;; setup virtualenvwrapper
(use-package virtualenvwrapper
  :init (setq venv-location "~/.virtualenvs/"))
;; wrap-region
(use-package wrap-region
  :init (wrap-region-global-mode t))
;;; yasnippet
(use-package yasnippet
  :init (yas-global-mode t))
;;; flyspell
(use-package flyspell
  :init (setq ispell-program-name "aspell" ; use aspell instead of ispell
	      ispell-extra-args '("--sug-mode=ultra")))
;;; whitespace
(use-package whitespace
  :init
  (progn
    (setq whitespace-line-column 80) ;; limit line length
    (setq whitespace-style '(face tabs empty trailing lines-tail))))
;;; uniquify
(use-package uniquify
  :init (setq uniquify-buffer-name-style 'forward))
;;; saveplace
(use-package saveplace
  :init
  (progn
    (setq-default save-place t)
    (setq save-place-file (concat user-emacs-directory "saved-places"))))

;;; load OS X configurations
(when (eq system-type 'darwin)
  (load-local "osx"))

;;; start server
(server-start)

;;; provide init package
(provide 'init)

;;; init.el ends here
