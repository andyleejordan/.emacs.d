;;; init --- Andrew Schwartzmeyer's Emacs init file
;;; Commentary:
;; See readme.

;;; Code:

;;; Package:
(setq gc-cons-threshold (* 10 1024 1024))

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

;; projectile
(use-package projectile
  :diminish projectile-mode
  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'helm
        projectile-switch-project-action 'helm-projectile)
  (projectile-global-mode))







;; highlight changes
(use-package git-gutter
  :diminish git-gutter-mode
  :config (global-git-gutter-mode))

  :config





;;; Formatting:
(setq-default indent-tabs-mode nil)
(setq sentence-end-double-space nil)







;;; functions
(defun compile-init ()
  "Byte recompile user Emacs directory."
  (interactive)
  (byte-recompile-directory user-emacs-directory))

(defun eval-region-or-buffer ()
  "Evaluate the selection, or, if empty, the whole buffer."
  (interactive)
  (let ((debug-on-error t))
    (cond
     (mark-active
      (call-interactively 'eval-region)
      (setq deactivate-mark t))
     (t
      (eval-buffer)))))

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

;;; extensions
;; adaptive word wrapping
(use-package adaptive-wrap
  :config (adaptive-wrap-prefix-mode))


  :config


;; company "complete anything"
(use-package company
  :diminish company-mode
  :commands (company-complete company-mode)
  :config
  (use-package company-c-headers)
  (push '(company-clang
          :with company-semantic
          :with company-yasnippet
          :with company-c-headers)
        company-backends))

;; automatic demangling
(use-package demangle-mode
  :commands demangle-mode)

;; dtrt
(use-package dtrt-indent
  :load-path "site-lisp/dtrt-indent"
  :config
  (dtrt-indent-mode)
  (setq dtrt-indent-min-quality 60
        dtrt-indent-verbosity 3))

;; flycheck
(use-package flycheck
  :diminish flycheck-mode
  :init (global-flycheck-mode))

;; flyspell - use aspell instead of ispell
(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :config (setq ispell-program-name (executable-find "aspell")
                ispell-extra-args '("--sug-mode=ultra")))

;; fortune
(use-package fortune-cookie
  :config
  (setq fortune-cookie-fortune-args "-s"
        fortune-cookie-cowsay-args "-f tux")
  (fortune-cookie-mode))

;; ggtags
(use-package ggtags
  :commands ggtags-mode
  :diminish ggtags-mode
  :config
  (general-define-key
   :keymaps 'ggtags-mode-map
   :states '(normal)
   "g d" 'helm-gtags-dwim)
  (use-package helm-gtags
    :commands (helm-gtags-dwim)
    :config (helm-gtags-mode)))


;; magit
(use-package magit
  :commands (magit-status projectile-vc)
  :config
  (use-package evil-magit)
  (add-to-list 'magit-log-arguments "--no-abbrev-commit")
  (setq magit-popup-use-prefix-argument 'default
        magit-completing-read-function 'magit-ido-completing-read))

(global-git-commit-mode)

;; saveplace
(use-package saveplace
  :config
  (setq-default save-place t
                save-place-file (f-expand "saved-places" user-emacs-directory)))
;; activate smartparens
(use-package smartparens
  :diminish smartparens-mode
  :init
  (use-package evil-smartparens
    :load-path "site-lisp/evil-smartparens"
    :diminish evil-smartparens-mode
    :config (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))
  (require 'smartparens-config)
  (smartparens-global-mode)
  (show-smartparens-global-mode)
  (smartparens-global-strict-mode))

;; tramp
(use-package tramp
  :config
  (setq tramp-verbose 9
        tramp-default-method "ssh"
        tramp-ssh-controlmaster-options
        (concat "-o ControlPath=/tmp/tramp.%%r@%%h:%%p "
                "-o ControlMaster=auto "
                "-o ControlPersist=no")))

;; undo-tree
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist
        `(("." . ,(f-expand "undo-tree" user-emacs-directory)))
        undo-tree-auto-save-history t))

;; unfill autofill
(use-package unfill
  :commands (unfill-region unfill-paragraph toggle-fill-unfill))

;; uniquify
(use-package uniquify
  :ensure nil
  :config (setq uniquify-buffer-name-style 'forward))

;; which-key
(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode))

;; whitespace
(use-package whitespace
  :commands (whitespace-mode)
  :config
  (setq whitespace-style '(face tabs spaces newline empty
                                trailing tab-mark newline-mark)))

(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :init (global-whitespace-cleanup-mode))

;;; syntax support
;; mode mappings
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.vcsh\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'magic-mode-alist '(";;; " . emacs-lisp-mode))

;; CMake
(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; C#
(use-package csharp-mode
  :mode "\\.cs$"
  :config (setq csharp-want-imenu nil))

(use-package dockerfile-mode
  :mode "Dockerfile.*\\'")

;; git modes
(use-package gitattributes-mode
  :disabled t)
(use-package gitconfig-mode
  :mode ("/\\.gitconfig\\'" "/\\.git/config\\'" "/git/config\\'" "/\\.gitmodules\\'"))
(use-package gitignore-mode
  :mode ("/\\.gitignore\\'" "/\\.git/info/exclude\\'" "/git/ignore\\'"))




;; json
(use-package json-mode
  :mode "\\.json$"
  :config (setq js-indent-level 4))

  :config


;; markdown
(use-package markdown-mode
  :mode ("\\.markdown\\'" "\\.mk?d\\'" "\\.text\\'")
  :init
  (evil-define-key 'normal markdown-mode-map
    (kbd "g d") 'markdown-jump
    (kbd "g x") 'markdown-follow-link-at-point))


;; nginx
(use-package nginx-mode
  :mode ("nginx.conf$" "/etc/nginx/.*"))

;; org mode extensions
(use-package org-plus-contrib
  :mode (("\\.org\\'" . org-mode) ("[0-9]\\{8\\}\\'" . org-mode))
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t) (gnuplot . t) (C . t) (emacs-lisp . t) (haskell . t)
     (latex . t) (ledger . t) (python . t) (ruby . t) (sh . t)))
  (evil-define-key 'normal org-mode-map (kbd "g x") 'org-open-at-point)
  :config
  (use-package evil-org)
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
        org-export-backends '(html beamer ascii latex md)))

;; powershell
(use-package powershell
  :mode ("\\.ps[dm]?1\\'" . powershell-mode)
  :config (add-hook 'powershell-mode-hook 'work-style))

;; puppet
(use-package puppet-mode
  :mode "\\.pp\\'")

;; ruby
(use-package ruby-mode)

;; rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :config (use-package flycheck-rust))

;; ssh-config
(use-package ssh-config-mode
  :mode ((".ssh/config\\'"       . ssh-config-mode)
         ("sshd?_config\\'"      . ssh-config-mode)
         ("known_hosts\\'"       . ssh-known-hosts-mode)
         ("authorized_keys2?\\'" . ssh-authorized-keys-mode)))
;;; Appearance:
(if (display-graphic-p)
    (progn
      (tool-bar-mode 0)
      (scroll-bar-mode 0)))

(use-package solarized-theme
  :config
  (setq solarized-use-variable-pitch nil)
  (load-theme 'solarized-dark t))

;; yaml
(use-package yaml-mode
  :mode "\\.ya?ml\'")
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
