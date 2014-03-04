;;; init --- Andrew Schwartzmeyer's Emacs init file

;;; Commentary:
;;; Fully customized Emacs configurations

;;; Code:

(add-to-list 'load-path "~/.emacs.d/")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(ace-jump-mode
		      ag
                      auto-complete
		      browse-kill-ring
		      dash-at-point
		      ein
		      exec-path-from-shell
		      expand-region
		      flx-ido
                      flycheck
		      gnuplot
		      haskell-mode
                      ido-ubiquitous
		      jabber
		      linum-relative
                      magit
                      markdown-mode
		      multi-term
		      mustache-mode
		      org
		      org-journal
		      org-pomodoro
                      projectile
                      puppet-mode
		      scratch
                      smartparens
                      smex
                      smooth-scroll
		      solarized-theme
                      undo-tree
                      virtualenvwrapper
		      yaml-mode
		      yasnippet))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;; ace-jump-mode
(require 'ace-jump-mode)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)

;;; auto-complete
(require 'auto-complete)
(global-auto-complete-mode t)

;;; auto-save
(setq auto-save-timeout 60)

;;; quit prompt
(setq confirm-kill-emacs 'yes-or-no-p)

;;; final-newline
(setq require-final-newline 't)

;;; column-number-mode
(setq column-number-mode t)

;;; no stinking disabled commands
(setq disabled-command-function nil)

;;; initial text mode
(setq initial-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; y/n for yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;;; scratch
(autoload 'scratch "scratch" nil t)

;;; activate expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;; browse-kill-ring
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;;; pull in shell path
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;; flycheck
(setq-default flycheck-clang-standard-library "libc++")
(setq-default flycheck-clang-language-standard "c++11")
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; dash-at-point
(define-key global-map (kbd "C-c d") 'dash-at-point)

;;; ein
(require 'ein)
(setq ein:use-auto-complete t)

;;; require ido-ubiquitous
(require 'ido)
(require 'ido-ubiquitous) ; replaces ido-everywhere

;;; ido-mode
(ido-mode t)

;;; flx-ido
(require 'flx-ido)
(flx-ido-mode t)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
;; increase garbage collection threshold
(setq gc-cons-threshold 20000000)

;;; inhibit startup message
(setq inhibit-startup-message t)

;;; linum-relative
(require 'linum-relative)

;;; magit
(define-key global-map (kbd "C-c C-g") 'magit-status)

;;; calendar
(setq calendar-week-start-day 1)

;;; org-mode
(setq org-agenda-files '("~/.org"))
(define-key global-map (kbd "C-c a") 'org-agenda)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(setq org-entities-user '(("join" "\\Join" nil "&#9285;" "" "" "⋈")
			  ("reals" "\\mathbb{R}" t "&#8477;" "" "" "ℝ")
			  ("ints" "\\mathbb{Z}" t "&#8484;" "" "" "ℤ")
			  ("models" "\\models" nil "&#8872;" "" "" "⊧")))
(require 'org-journal)

;;; activate projectile
(require 'projectile)
(projectile-global-mode)
(define-key projectile-mode-map [?\s-d] 'projectile-find-dir)
(define-key projectile-mode-map [?\s-p] 'projectile-switch-project)
(define-key projectile-mode-map [?\s-f] 'projectile-find-file)
(define-key projectile-mode-map [?\s-a] 'projectile-ag)

;;; multi-term
(require 'multi-term)
(setq multi-term-program "/bin/zsh")

;;; sage
(if (eq system-type 'darwin)
    (add-to-list 'load-path "/Applications/Sage.app/Contents/Resources/sage/data/emacs"))
(ignore-errors
  (require 'sage "sage")
  (setq sage-command "~/bin/sage"))

;;; activate smartparens
(smartparens-global-mode t)
(sp-local-pair '(emacs-lisp-mode erc-mode git-commit-mode
		 org-mode text-mode) "'" nil :actions nil)
;;; setup smex bindings
(require 'smex)
(setq smex-save-file (expand-file-name ".smex-items" "~/.emacs.d/"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;; scrolling
(require 'smooth-scroll)
(smooth-scroll-mode t)
(setq smooth-scroll/vscroll-step-size 8)

;;; activate solarized-dark theme
(setq solarized-use-variable-pitch nil)
(setq solarized-high-contrast-mode-line t)
(load-theme 'solarized-dark t)

;;; undo-tree
(global-undo-tree-mode t)

;;; setup virtualenvwrapper
(require 'virtualenvwrapper)
(setq venv-location "~/.virtualenvs/")

;;; personal functions

;;; setting auto-mode-alist
;; set arduino *.ino files to c-mode
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))

;;; select whole line
(defun select-whole-line ()
  "Select whole line which has the cursor."
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))
(global-set-key (kbd "C-c l") 'select-whole-line)

;;; comment/uncomment line/region
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region-or-line)

;;; yasnippet
(require 'yasnippet)
(yas-global-mode t)

;;; emacs configurations

;;; disable bell function
(setq ring-bell-function 'ignore)

;;; erc, configured with help from
;;;; http://emacs-fu.blogspot.com/2009/06/erc-emacs-irc-client.html
(load "~/.ercpass")
(require 'erc)
(require 'tls)
(require 'erc-services)
(require 'erc-notify)
(erc-services-mode t)
(erc-notify-mode t)
(erc-spelling-mode t) ;; flyspell

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

;; switch to ERC with Ctrl+c e
(global-set-key (kbd "C-c e") 'erc-start-or-switch)

;;; eval-buffer
(global-set-key (kbd "C-c C-x") 'eval-buffer)

;;; set auto revert of buffers if file is changed externally
(global-auto-revert-mode t)

;;; compile shortcut
(global-set-key (kbd "C-x c") 'compile)

;;; backups
(setq backup-by-copying t)

(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(setq backup-directory-alist `(("." . ,(concat
					user-emacs-directory "backups"))))

;;; blink cursor
(blink-cursor-mode t)

;;; flyspell
(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))

;;; subword mode
(global-subword-mode t)

;;; visual-line-mode
(global-visual-line-mode t)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;;; whitespace
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))

;;; below stolen from better-defaults

;;; disable toolbar and scrollbar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(add-hook 'ibuffer-mode-hook (lambda () (setq truncate-lines t)))

;;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;; saveplace

(require 'saveplace)
(setq-default save-place t)

;;; better keys
(global-set-key (kbd "M-/") 'hippie-expand)

;;; isearch
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;;; matching parentheses
(show-paren-mode t)

;;; symlink version-control follow
(setq vc-follow-symlinks t)

;;; start server
(server-start)

;;; provide init package
(provide 'init)

;;; init.el ends here
