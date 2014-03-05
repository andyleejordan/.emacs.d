;;; init --- Andrew Schwartzmeyer's Emacs init file

;;; Commentary:
;;; Fully customized Emacs configurations

;;; Code:

;;; Cask setup
(require 'cask "~/.cask/cask.el")
(cask-initialize)

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
;; comment/uncomment line/region
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

;;; erc --- configured with help from:
;; http://emacs-fu.blogspot.com/2009/06/erc-emacs-irc-client.html
(load "~/.ercpass")
(require 'erc)
(require 'tls)
(require 'erc-services)
(require 'erc-notify)
(erc-services-mode t)
(erc-notify-mode t)
(erc-spelling-mode t) ;; flyspell
;; notify list
(setq erc-notify-list '("p_nathan1"))
;; reduce notifications
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
(setq erc-autojoin-timing 'ident)
;; start or switch to buffer function
(defun erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "chat.freenode.net:7000") ;; ERC already active?
      (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
      (erc-tls :server "chat.freenode.net" :port
	       7000 :nick "andschwa" :full-name "Andrew Schwartzmeyer"))))

;;; org-mode
;; org-journal
(require 'org-journal)
(setq org-journal-dir "~/Documents/personal/journal/")
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

;;; packages

;; ace-jump-mode
(require 'ace-jump-mode)
(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
;; auto-complete
(require 'auto-complete)
(global-auto-complete-mode t)
;; scratch
(autoload 'scratch "scratch" nil t)
;; activate expand-region
(require 'expand-region)
;; browse-kill-ring
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
;; pull in shell path
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
;; flycheck
(setq-default flycheck-clang-standard-library "libc++")
(setq-default flycheck-clang-language-standard "c++11")
(add-hook 'after-init-hook #'global-flycheck-mode)
;; ein
(require 'ein)
(setq ein:use-auto-complete t)
;; require ido-ubiquitous
(require 'ido)
(require 'ido-ubiquitous) ; replaces ido-everywhere
;; ido-mode
(ido-mode t)
;; ido-vertical
(ido-vertical-mode t)
;; flx-ido
(require 'flx-ido)
(flx-ido-mode t)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
;; increase garbage collection threshold
(setq gc-cons-threshold 20000000)
;; inhibit startup message
(setq inhibit-startup-message t)
;; linum-relative
(require 'linum-relative)
;; activate projectile
(require 'projectile)
(projectile-global-mode)
;; move-text
(require 'move-text)
(move-text-default-bindings)
;; multiple-cursors
(require 'multiple-cursors)
;; popwin
(require 'popwin)
(popwin-mode 1)

;; activate smartparens
(smartparens-global-mode t)
(sp-local-pair '(emacs-lisp-mode erc-mode git-commit-mode
		 org-mode text-mode) "'" nil :actions nil)
;; setup smex bindings
(require 'smex)
(setq smex-save-file (expand-file-name ".smex-items" "~/.emacs.d/"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
;; scrolling
(require 'smooth-scroll)
(smooth-scroll-mode t)
(setq smooth-scroll/vscroll-step-size 8)
;; undo-tree
(global-undo-tree-mode t)
;; setup virtualenvwrapper
(require 'virtualenvwrapper)
(setq venv-location "~/.virtualenvs/")
;; wrap-region
(require 'wrap-region)
(wrap-region-global-mode t)


;;; yasnippet
(require 'yasnippet)
(yas-global-mode t)




;;; flyspell
(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))

;;; whitespace
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))

;;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(add-hook 'ibuffer-mode-hook (lambda () (setq truncate-lines t)))

;;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;; saveplace
(require 'saveplace)
(setq-default save-place t)

;;; shortcuts
;; miscellaneous
(define-key global-map (kbd "M-/") 'hippie-expand)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(define-key global-map (kbd "C-=") 'er/expand-region)
(define-key global-map (kbd "C-z") popwin:keymap)
(define-key global-map (kbd "C-x c") 'compile)
(define-key global-map (kbd "C-x g") 'magit-status)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'comment-or-uncomment-region-or-line)
(define-key global-map (kbd "C-c d") 'dash-at-point)
(define-key global-map (kbd "C-c e") 'erc-start-or-switch)
(define-key global-map (kbd "C-c l") 'select-whole-line)
(define-key global-map (kbd "C-c x") 'eval-buffer)
;; isearch
(define-key global-map (kbd "C-s") 'isearch-forward-regexp)
(define-key global-map (kbd "C-r") 'isearch-backward-regexp)
(define-key global-map (kbd "C-M-s") 'isearch-forward)
(define-key global-map (kbd "C-M-r") 'isearch-backward)
;; projectile
(define-key projectile-mode-map [?\s-d] 'projectile-find-dir)
(define-key projectile-mode-map [?\s-p] 'projectile-switch-project)
(define-key projectile-mode-map [?\s-f] 'projectile-find-file)
(define-key projectile-mode-map [?\s-a] 'projectile-ag)
;; multiple cursors
(define-key global-map (kbd "C-S-c C-S-c") 'mc/edit-lines)
(define-key global-map (kbd "C->") 'mc/mark-next-like-this)
(define-key global-map (kbd "C-<") 'mc/mark-previous-like-this)
(define-key global-map (kbd "C-c C-<") 'mc/mark-all-like-this)

;;; load OS X configurations
(when (eq system-type 'darwin)
  (load-local "osx"))

;;; start server
(server-start)

;;; provide init package
(provide 'init)

;;; init.el ends here
