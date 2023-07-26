;;; init.el --- Andy Jordans's Emacs customizations. -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2023 Andy Jordan

;; Author: Andy Jordan <2226434+andyleejordan@users.noreply.github.com>
;; Created: 30 Aug 2013
;; Homepage: https://github.com/andschwa/.emacs.d

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is my `init.el', there are many like it, but this is my own.
;; It is constantly evolving, and attempts to make use of the best
;; packages and best practices available. GNU Emacs is my favorite
;; piece of software: I would not be the programmer I am today without
;; GNU Emacs. Please take as much or as little from it as you need.

;;; Code:

;;; Package System:

(when (< emacs-major-version 27)
  (load-file (expand-file-name "early-init.el" user-emacs-directory))
  (package-initialize))

(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))

(customize-set-variable 'use-package-enable-imenu-support t)
(customize-set-variable 'use-package-always-ensure t)

(defmacro use-feature (name &rest args)
  "Like `use-package' for NAME and ARGS, but with `:straight' nil."
  (declare (indent defun))
  `(use-package ,name :ensure nil ,@args))

;; Lisp list, string, and file extensions.
(use-package dash)
(use-package s)
(use-package f)

;; Save data files consistently.
(use-package no-littering)

;; My own "package" of extensions.
(use-feature andy :load-path "etc")

(customize-set-variable
 'custom-file (no-littering-expand-etc-file-name "custom.el"))

(use-feature package
  :custom
  (package-quickstart t)
  (package-quickstart-file (no-littering-expand-var-file-name "package-quickstart.el")))

;; Way easier key binding.
(use-package bind-key)

;; Intelligently hides minor modes.
(use-package delight)

;;; Platform:

(use-feature linux
  :load-path "etc"
  :if (eq system-type 'gnu/linux))

(use-feature osx
  :load-path "etc"
  :if (eq system-type 'darwin))

(use-feature windows
  :load-path "etc"
  :if (eq system-type 'windows-nt))

(use-feature local
  :load-path "etc"
  :if (file-readable-p (no-littering-expand-etc-file-name "local.el")))

;;; Cursor and Mark Movement:

(bind-key "M-o" #'other-window)

(use-package copy-as-format
  :bind ("C-c M-w" . copy-as-format)
  :custom (copy-as-format-default "github"))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package git-link)

(use-feature saveplace
  :config
  (or (call-if-fbound #'save-place-mode)
      (call-if-fbound #'save-place)))

(use-feature subword
  :config (global-subword-mode))

;; Also see `set-selective-display'.
(use-feature hideshow
  :delight hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
              ("C-c h" . hs-toggle-hiding+)
              ("C-c l" . hs-hide-level)
              :filter (or (hs-looking-at-block-start-p)
                          (hs-already-hidden-p)
                          (bobp))
              ([tab] . hs-toggle-hiding+))
  :custom (hs-allow-nesting t))

;;; Windows / Frames and the buffers in them:

(use-feature tab-bar
  :bind (("C-x t [" . tab-bar-history-back)
         ("C-x t ]" . tab-bar-history-forward)
         ("C-x t n" . tab-new) ; `C-x t 2’
         ("C-x t w" . tab-close) ; `C-x t 0’
         ("C-x t u" . tab-undo) ; `C-x t 0’
         ("M-<right>" . tab-next) ; `tab-bar-switch-to-prev-tab’
         ("M-<left>" . tab-previous)) ; `tab-bar-switch-to-next-tab’
  :custom (tab-bar-close-button-show nil)
  :config (tab-bar-history-mode) (tab-bar-mode))

(use-package buffer-move)

(use-package transpose-frame
  :config (bind-key "C-x 4 t" #'transpose-frame))

(use-feature windmove ; `S-<left,right,up,down>' to move windows
  :config (windmove-default-keybindings))

(use-feature window
  :no-require
  :custom
  ;; I want rg and magit buffers to open in the other window, if it’s to the right
  (display-buffer-alist
   '((".*"
      (display-buffer-reuse-window display-buffer-same-window)
      (reusable-frames . t))))
  (even-window-sizes t))

(use-feature winner ; `C-c <left,right>' to undo/redo windows
  :disabled ; `tab-bar-history-mode’ replaces this.
  :config (winner-mode))

;;; Minibuffer Interface:

(bind-key* [remap keyboard-quit] #'keyboard-quit-context+)

(use-feature icomplete
  :if (fboundp 'fido-vertical-mode)
  :custom
  (icomplete-compute-delay 0)
  (icomplete-separator (with-face " | " :inherit 'shadow))
  :config
  ;; Use `isearch' instead of regexp, especially since `C-s' and `C-r'
  ;; are bound like in `ido' to move through candidates.
  (bind-key "M-s" #'isearch-forward icomplete-fido-mode-map)
  (bind-key "M-r" #'isearch-backward icomplete-fido-mode-map)
  (bind-key "/" #'icomplete-fido-slash minibuffer-local-filename-completion-map)
  (use-package orderless
    :custom (orderless-matching-styles
             '(orderless-regexp orderless-prefixes orderless-initialism)))
  (defun fido-mode+ ()
    (setq-local completion-styles '(orderless)
                truncate-lines t))
  (add-hook 'icomplete-minibuffer-setup-hook #'fido-mode+)
  (fido-vertical-mode))

(use-feature minibuffer
  :custom
  (enable-recursive-minibuffers t)
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completions-format 'vertical)
  (completion-cycle-threshold 3)
  (minibuffer-beginning-of-buffer-movement t)
  (minibuffer-message-clear-timeout 2)
  :config
  (bind-key "SPC" #'icomplete-fido-ret minibuffer-local-filename-completion-map)
  (bind-key "SPC" #'icomplete-fido-ret minibuffer-local-filename-must-match-map))

(use-package consult
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  :config
  (bind-key [remap yank-pop] #'consult-yank-pop)
  (bind-key [remap switch-to-buffer] #'consult-buffer)
  (bind-key "C-x C-r" #'consult-recent-file))

(use-package eldoc :delight) ; built-in but in GNU ELPA

(use-feature mb-depth
  :config (minibuffer-depth-indicate-mode))

(use-feature minibuf-eldef
  :custom (minibuffer-eldef-shorten-default t)
  :config (minibuffer-electric-default-mode))

(use-feature savehist
  :custom (history-delete-duplicates)
  :config (savehist-mode))

(use-feature which-func
  :custom (which-func-unknown "")
  :config (which-function-mode))

(use-package which-key
  :delight
  :config
  (bind-key "C-c w" #'which-key-show-major-mode)
  (which-key-mode))

;;; Version Control:

(use-package diff-hl
  :custom (diff-refine 'navigation)
  :config
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (global-diff-hl-mode))

(use-package magit
  :defines magit-dwim-selection magit-section-initial-visibility-alist
  ;; Now defines global bindings automatically. See #4237.
  :bind ("C-c g" . magit-file-dispatch) ; But this one is recommended.
  :config (add-to-list 'magit-dwim-selection '(magit-branch-and-checkout nil t))
  :custom
  (magit-save-repository-buffers 'dontask)
  (magit-published-branches nil "Disable confirmation.")
  (magit-diff-refine-hunk 'all "Word diffs.")
  (magit-prefer-remote-upstream t)
  (magit-status-initial-section '(((unstaged) (status)) ((staged) (status)) 1))
  (magit-section-initial-visibility-alist
   '((untracked . hide) (unpushed . hide) (branch . hide) (stashes . hide)))
  ;; TODO: Set `magit-display-buffer-function'.
  (magit-bury-buffer-function #'magit-mode-quit-window
                              "Stop restoring windows.")
  (magit-no-message '("Turning on magit-auto-revert-mode...")))

(use-package git-commit
  :custom (git-commit-major-mode 'markdown-mode)
  :config
  (add-hook 'git-commit-mode-hook (lambda () (set-fill-column 72)))
  (add-to-list 'git-commit-style-convention-checks 'overlong-summary-line)
  (global-git-commit-mode))

(use-feature vc-hooks
  :custom
  (vc-ignore-dir-regexp
   (format "\\(%s\\)\\|\\(%s\\)"
           vc-ignore-dir-regexp
           tramp-file-name-regexp)))

;;; Buffers:

(bind-key [remap kill-buffer] #'kill-this-buffer)

(use-feature ibuffer
  ;; TODO: Make the groups reasonable.
  :bind ([remap list-buffers] . ibuffer)
  :custom
  ;; See `ibuffer-filtering-alist’ for filters
  (ibuffer-saved-filter-groups ; Not currently in use.
   '(("default"
      ("Search" (or (mode . occur-mode)
                    (mode . rg-mode)
                    (mode . grep-mode)
                    (mode . xref--xref-buffer-mode)))
      ("Emacs" (or (filename . ".emacs.d")
                   (derived-mode . emacs-lisp-mode)
                   (mode . Custom-mode)))
      ("Code" (or (derived-mode . prog-mode)
                  (mode . compilation-mode)))
      ("Dotfiles" (or (filename . "dotfiles")
                      (filename . ".dotfiles")))
      ("Help" (or (mode . help-mode)
                  (mode . helpful-mode)
                  (mode . man-mode)
                  (mode . woman-mode)))
      ("Logs" (or (mode . special-mode)
                  (mode . messages-buffer-mode)
                  (mode . fundamental-mode))))))
  (ibuffer-formats
   '((mark modified read-only locked " "
           (mode 15 15 :right :elide) " "
           (name 35 -1 :left) " "
           filename-and-process)
     (mark " " (name 16 -1) " " filename)))
  (ibuffer-expert t)
  (ibuffer-use-other-window t)
  (ibuffer-default-sorting-mode 'filename/process)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-filter-group-name-face 'font-lock-doc-face)
  :config
  (add-args-to-list 'ibuffer-help-buffer-modes
                    '(helpful-mode man-mode woman-mode))
  (add-to-list 'ibuffer-fontification-alist
               '(50 (ibuffer-buffer-file-name) font-lock-builtin-face))
  (defun ibuffer-mode+ ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (hl-line-mode))
  (add-hook 'ibuffer-mode-hook #'ibuffer-mode+))

(use-package ibuffer-vc)

(use-feature uniquify
  :custom (uniquify-buffer-name-style 'forward))

;;; File Navigation:

(bind-key "C-c i" #'find-init-el)

(use-feature dired
  :config (bind-key [remap list-directory] #'dired)
  :custom
  (dired-dwim-target t "Enable side-by-side `dired' buffer targets.")
  (dired-recursive-copies 'always "Better recursion in `dired'.")
  (dired-recursive-deletes 'top)
  (dired-listing-switches "-alFhv" "Must not contain `-p'."))

;; https://www.emacswiki.org/emacs/DiredPlus
(use-feature dired+ :disabled
  ;; Use `C-M-R’ to toggle reuse of buffers.
  :quelpa (dired+ :fetcher url :url "https://www.emacswiki.org/emacs/download/dired+.el"))

(use-package dired-du ; On `C-x M-r’
  :custom (dired-du-size-format t))

(use-package dired-git-info
  :config
  (bind-key ")" #'dired-git-info-mode dired-mode-map))

(use-feature dired-x
  :config
  (bind-key "C-x C-j" #'dired-jump)
  (bind-key "C-x 4 C-j" #'dired-jump-other-window))

(use-package project ; built-in but in GNU ELPA
  :defines project-find-functions
  :config
  ;; Similar to project-try-vc but works when VC is disabled.
  (defun project-try-magit (dir)
    ;; TODO: Send this upstream so it doesn’t break again.
    (require 'magit-process)
    (let* ((root (magit-toplevel dir)))
      (and root (cons 'vc root))))
  (add-to-list 'project-find-functions #'project-try-magit t)
  ;; Use Magit instead of built-in VC.
  (when (boundp 'project-switch-commands)
    (setf (alist-get ?v project-switch-commands) '("Magit" magit-status))))

(use-feature recentf
  :custom
  (recentf-max-saved-items 200)
  (recentf-auto-cleanup 'never "Disabled for performance with Tramp.")
  :config
  (add-to-list 'recentf-exclude
               (lambda (f) (not (string= (file-truename f) f))))
  ;; Save when idle, because Emacs crashes (or I kill it).
  (run-with-idle-timer (* 5 60) :repeat #'recentf-save-list)
  (recentf-mode))

;;; Searching:

(use-feature imenu
  :custom
  (imenu-auto-rescan t)
  (imenu-max-item-length nil)
  :config (bind-key "M-i" #'imenu))

(bind-key "M-s g" #'vc-git-grep)

(use-feature isearch
  :bind (("M-s M-o" . multi-occur)
         :map minibuffer-local-isearch-map
         ("M-/" . isearch-complete-edit)
         :map isearch-mode-map
         ("M-/" . isearch-complete)
         ([remap isearch-abort] . isearch-cancel))
  :custom
  (search-whitespace-regexp ".*?")
  (isearch-allow-scroll t)
  (isearch-lazy-count t))

;; https://www.emacswiki.org/emacs/IsearchPlus
(use-feature isearch+ :disabled
  :quelpa (isearch+ :fetcher url :url "https://www.emacswiki.org/emacs/download/isearch+.el"))

(use-feature grep
  :config (bind-key "M-s R" #'rgrep)) ; or `rg'

(use-package rg ; `ripgrep'
  :config
  ;; Also see `vc-git-grep' and `project-find-regexp'.
  (bind-key "M-s r" #'rg) ; or `rgrep'
  (bind-key "M-s p" #'rg-project) ; or `project-find-regexp'
  (bind-key "M-s s" #'rg-ask-dwim)
  (rg-define-search rg-ask-dwim
    :query ask :format regexp
    :files "everything" :dir project))

;; Use the same binding as `occur-edit-mode' for other writable modes.
;; Can always initiate with `e' and exit with `C-c C-c' or `C-x C-s'.
;;
;; I have no idea why these are otherwise scattered across `e', `C-x
;; C-q', and `C-c C-p'.

(use-feature replace
  :config (bind-key "C-x C-s" #'occur-cease-edit occur-edit-mode-map))

;; https://www.emacswiki.org/emacs/ReplacePlus
(use-feature replace+ :disabled
  :quelpa (replace+ :fetcher url :url "https://www.emacswiki.org/emacs/download/replace+.el"))

(use-feature wdired
  :config (bind-key "e" #'dired-toggle-read-only dired-mode-map)
  :custom (wdired-allow-to-change-permissions t))

(use-package wgrep ; makes `rg' buffers writable too
  :defines grep-mode-map ; kinda
  :config (bind-key "e" #'wgrep-change-to-wgrep-mode grep-mode-map)
  :custom (wgrep-auto-save-buffer t))

;;; Formatting / Indentation / Whitespace:

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-feature align
  :config (bind-key "C-x \\" #'align-regexp))

(use-package clang-format
  :requires cc-mode
  :defines c-mode-base-map
  :config (bind-key [remap indent-region] #'clang-format-region c-mode-base-map))

(use-package dtrt-indent
  :delight
  :defines dtrt-indent-hook-mapping-list
  :custom
  (dtrt-indent-verbosity 0)
  (dtrt-indent-min-quality 60)
  :config
  (add-args-to-list 'dtrt-indent-hook-mapping-list
                    '((powershell-mode c/c++/java powershell-indent)
                      (groovy-mode default groovy-indent-offset)))
  (dtrt-indent-global-mode))

(use-package editorconfig
  :delight
  :config (editorconfig-mode))

(use-feature indent
  :no-require
  :custom
  (indent-tabs-mode nil)
  (tab-always-indent 'complete))

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Comment-Commands.html
(use-feature newcomment
  :custom (comment-fill-column 0))

(use-feature paragraphs
  :no-require
  :custom (sentence-end-double-space nil))

(use-package whitespace-cleanup-mode
  :delight
  :config (global-whitespace-cleanup-mode))

;;; Editing:

(customize-set-variable 'truncate-lines t)
(bind-key [remap delete-char] #'delete-forward-char)
(bind-key "C-x w" #'toggle-truncate-lines)
(bind-key "C-M-y" #'raise-sexp)
(bind-key "C-M-<backspace>" #'delete-pair)

(use-feature autoinsert
  :config
  (auto-insert-mode)
  (define-auto-insert
    '(sh-mode . "Bash skeleton")
    [(lambda () (sh-set-shell "bash" t nil))
     '(()
       "#!/bin/bash" \n
       \n
       "set -o errexit" \n
       "set -o pipefail" "\n\n")]))

(use-feature autorevert
  :delight auto-revert-mode
  :custom (auto-revert-avoid-polling t)
  :config (global-auto-revert-mode))

(use-package auto-sudoedit
  :delight
  :commands auto-sudoedit-sudoedit
  :init (defalias 'sudoedit #'auto-sudoedit-sudoedit))

(use-feature delsel
  :config (delete-selection-mode))

(use-feature ediff
  :custom
  (ediff-keep-variants nil)
  (ediff-show-clashes-only t)
  (ediff-diff-options "-w")
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-feature electric
  :custom ; “Prettier ‘quotes’”
  (electric-quote-replace-double t)
  (electric-quote-context-sensitive t)
  :config
  (electric-quote-mode)
  (add-to-list 'electric-quote-inhibit-functions
               (lambda () (eq major-mode 'yaml-mode))))

(use-feature elec-pair
  :custom (electric-pair-skip-whitespace 'chomp)
  :config (electric-pair-mode))

(use-package undo-fu
  :delight
  ;; Backports Emacs 28's `undo-redo'.
  :bind (("C-/" . undo-fu-only-undo)
         ("C-?" . undo-fu-only-redo)
         ("C-c C-?" . undo-fu-only-redo-all))
  :custom (undo-fu-allow-undo-in-region t))

(use-package undo-fu-session
  :config (global-undo-fu-session-mode))

(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))

;;; Tab Completion:

(use-feature hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand)
  :config
  ;; This doesn't use `custom' because it is autoloaded for
  ;; "historical reasons" and we want to defer loading.
  (setq hippie-expand-try-functions-list
        '(try-expand-all-abbrevs
          try-expand-dabbrev-visible
          try-expand-dabbrev ; this buffer
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-expand-whole-kill
          try-complete-file-name-partially
          try-complete-file-name)))

;;; Syntax Checking:

;; Treat backquotes as pairs in text mode.
(use-feature text-mode
  :config
  (modify-syntax-entry ?\` "$`" text-mode-syntax-table))

(use-package flymake ; built-in but in GNU ELPA
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error))
  :config
  ;; Magic from https://stackoverflow.com/a/53858408/1028665
  (defun flymake--transform-mode-line-format (ret)
    "Change the output of `flymake--mode-line-format'."
    (setf (seq-elt (car ret) 1) " Fly") ret)
  (advice-add #'flymake--mode-line-format
              :filter-return #'flymake--transform-mode-line-format))

(use-package flymake-shellcheck
  :hook (sh-mode . flymake-shellcheck-load))

;;; Tags:

(use-package dumb-jump
  :config (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Alternatives include: eglot, irony, cquery, rtags, ggtags, and ycmd.
(use-package eglot ; an LSP client in GNU ELPA
  :hook
  (c-mode-common . eglot-ensure)
  (python-mode . eglot-ensure)
  (rust-mode . eglot-ensure)
  (yaml-mode . eglot-ensure)
  :custom
  (eglot-auto-display-help-buffer t)
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil)
  :config
  (bind-key "C-c r" #'eglot-rename eglot-mode-map)
  ;; Install with ‘npm install -g yaml-language-server’
  (add-to-list 'eglot-server-programs '(yaml-mode . ("yaml-language-server" "--stdio")))
  (defun eglot-format-buffer-on-save ()
    (if (and (project-current) (eglot-managed-p))
        (add-hook 'before-save-hook #'eglot-format-buffer nil 'local)
      (remove-hook 'before-save-hook #'eglot-format-buffer 'local)))
  (add-hook 'eglot-managed-mode-hook #'eglot-format-buffer-on-save))

(use-package xref) ; built-in but in GNU ELPA

;;; Spelling:

(use-feature flyspell
  ;; Disable on Windows because `aspell' 0.6+ isn't available.
  :if (executable-find "aspell")
  :unless (eq system-type 'windows-nt)
  :delight
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  :custom
  (flyspell-mode-map (make-sparse-keymap) "Disable all flyspell bindings")
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra")))

(use-package flyspell-correct
  :defines flyspell-mode-map
  :requires flyspell
  :config (bind-key [remap ispell-word] #'flyspell-correct-wrapper flyspell-mode-map))

(use-package auto-correct
  :delight
  :hook (flyspell-mode . auto-correct-mode)
  :custom (flyspell-use-global-abbrev-table-p t))

;;; Tools:

(use-feature apropos
  :custom (apropos-do-all t))

(use-feature bookmark
  :custom
  (bookmark-version-control t)
  (bookmark-save-flag 1))

;; https://www.emacswiki.org/emacs/BookmarkPlus
(use-feature bookmark+ :disabled
  :init (install-wiki-package '("bookmark+.el"
                                "bookmark+-mac.el"
                                "bookmark+-bmu.el"
                                "bookmark+-1.el"
                                "bookmark+-key.el"
                                "bookmark+-doc.el"
                                "bookmark+-chg.el")))

(use-feature compile
  :custom
  (compilation-ask-about-save nil)
  (compilation-scroll-output t)
  (compilation-always-kill t)
  (compilation-error-regexp-alist
   (delete 'maven compilation-error-regexp-alist))
  :config
  (defun colorize-compilation ()
    (require 'ansi-color)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook #'colorize-compilation))

(use-package default-text-scale)

(use-package demangle-mode)

(use-package easy-hugo
  :custom
  (easy-hugo-basedir "~/blog")
  (easy-hugo-url "https://andschwa.com")
  (easy-hugo-postdir "content/posts"))

(use-package exec-path-from-shell
  :if (or (daemonp) (display-graphic-p))
  :custom
  (exec-path-from-shell-shell-name "bash")
  (exec-path-from-shell-arguments '("-l"))
  :config
  (add-to-list 'exec-path-from-shell-variables "SSH_AUTH_SOCK")
  (exec-path-from-shell-initialize))

(use-feature eshell
  :bind ("C-c e" . eshell)
  :custom
  (eshell-visual-commands '("bash" "htop" "fish"))
  (eshell-highlight-prompt nil)
  (eshell-prompt-function
   (lambda ()
     (let ((red     "#dc322f")
           (magenta "#d33682")
           (blue    "#268bd2")
           (cyan    "#2aa198")
           (green   "#859900")
           (base    "#839496"))
       (concat
        (let ((status eshell-last-command-status))
          (when (not (= status 0))
            (with-face (concat (number-to-string status) " ") :foreground magenta)))
        (with-face "@" :foreground (if (= (user-uid) 0) red blue))
        (with-face (car (s-split "\\." (system-name))) :foreground base) " "
        (with-face (let ((path (replace-regexp-in-string (concat "\\`" (getenv "HOME")) "~" (eshell/pwd))))
                     (s-reverse (s-truncate 15 (s-reverse path) "…")))
                   :foreground blue) " "
        (let ((head (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))
          (unless (string-match "fatal:" head)
            (concat (with-face (replace-regexp-in-string "\n\\'" "" head) :foreground green)))) " "
        (with-face "$" :foreground cyan) " ")))))

(use-feature gud
  :no-require
  :custom (gdb-many-windows t))

(use-feature ielm
  :custom (ielm-prompt "> "))

(use-package lorem-ipsum
  :custom (lorem-ipsum-sentence-separator " "))

(use-feature man
  :bind ("C-c m" . man))

(use-package org
  :ensure org-plus-contrib
  :config
  (add-hook 'org-mode-hook #'turn-on-auto-fill)
  (require 'org-tempo) ; Bring back `<s [TAB]'.
  (add-args-to-list 'org-structure-template-alist
                    '(("el" . "src emacs-lisp")
                      ("sh" . "src sh")))
  :custom
  (org-startup-indented nil)
  (org-src-tab-acts-natively t)
  (org-adapt-indentation nil)
  (org-catch-invisible-edits 'smart)
  (org-latex-listings t)
  (org-pretty-entities t)
  (org-latex-custom-lang-environments '((C "lstlisting")))
  (org-entities-user '(("join" "\\Join" nil "&#9285;" "" "" "⋈")
                       ("reals" "\\mathbb{R}" t "&#8477;" "" "" "ℝ")
                       ("ints" "\\mathbb{Z}" t "&#8484;" "" "" "ℤ")
                       ("complex" "\\mathbb{C}" t "&#2102;" "" "" "ℂ")
                       ("models" "\\models" nil "&#8872;" "" "" "⊧")))
  (org-export-backends '(html beamer ascii latex md))
  (org-babel-load-languages '((emacs-lisp . t)
                              (shell . t))))

(use-feature re-builder
  :custom (reb-re-syntax 'string))

(use-package restart-emacs
  :bind ("C-c Q" . restart-emacs))

(use-feature shell
  :custom (shell-command-prompt-show-cwd t))

(use-package system-packages)

(use-package xr)

;;; Appearance:

(use-feature frame
  :custom (blink-cursor-blinks 0 "Blink forever.")
  :config
  ;; Set this for every made frame, and for the initial frame. This
  ;; handles running both `emacs’ directly and `emacsclient’. We don’t
  ;; set this as part of `default-frame-alist’ in `early-init.el’
  ;; because we need: the fonts to be available as part of the display
  ;; system, `font-size’ to be set to its final value (which can
  ;; happen in `local.el’), `set-preferred-font’ to be loaded from
  ;; `andy.el’, and it uses `--map-first’ from `dash.el’.
  (blink-cursor-mode)
  (add-hook 'after-make-frame-functions #'set-preferred-font)
  (add-hook 'after-init-hook #'set-preferred-font))

(use-package fortune-cookie
  :custom
  (fortune-cookie-fortune-string
   "History repeats itself:\nthe first time as tragedy,\nthe second time as farce.")
  (fortune-cookie-cowsay-enable (executable-find "cowsay"))
  (fortune-cookie-cowsay-args '("-f" "tux"))
  :config (fortune-cookie-mode))

;; https://www.emacswiki.org/emacs/HelpPlus
(use-feature help+ :disabled
  :quelpa (help+ :fetcher url :url "https://www.emacswiki.org/emacs/download/help+.el"))

(use-feature help-fns+ :disabled
  :quelpa (help-fns+ :fetcher url :url "https://www.emacswiki.org/emacs/download/help-fns+.el"))

(use-feature help-macro+ :disabled
  :quelpa (help-macro+ :fetcher url :url "https://www.emacswiki.org/emacs/download/help-macro+.el"))

(use-feature help-mode+ :disabled
  :quelpa (help-mode+ :fetcher url :url "https://www.emacswiki.org/emacs/download/help-mode+.el"))

;; https://www.emacswiki.org/emacs/InfoPlus
(use-feature info+ :disabled
  :quelpa (info+ :fetcher url :url "https://www.emacswiki.org/emacs/download/info+.el"))

(use-package hl-todo
  :defines hl-todo-keyword-faces
  :config
  (add-to-list 'hl-todo-keyword-faces
               `("ANDY" . ,(cdr (assoc "HACK" hl-todo-keyword-faces))))
  (add-to-list 'hl-todo-keyword-faces
               `("DEMO" . ,(cdr (assoc "PROG" hl-todo-keyword-faces))))
  (global-hl-todo-mode))

(use-package ligature
  :if (file-readable-p (no-littering-expand-etc-file-name "ligature.el"))
  :load-path "etc/ligature.el"
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "!=" "!!" ">:" "=>"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  (global-ligature-mode))

(use-feature paren
  :custom
  (show-paren-delay 0)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config (show-paren-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode) ; highlight color codes like "#aabbcc"

(use-package solarized-theme
  :custom
  (solarized-theme-dir (no-littering-expand-var-file-name "themes/"))
  (solarized-scale-org-headlines nil)
  (solarized-scale-outline-headlines nil)
  (solarized-use-variable-pitch nil)
  (solarized-use-more-italic t)
  (x-underline-at-descent-line t)
  :config
  (add-to-list 'custom-theme-load-path solarized-theme-dir))

(use-feature solarized-andy
  :load-path "etc"
  :after solarized-theme
  :config
  ;; Schedule (has to start at midnight, then switch):
  (run-at-time "12:00am" (* 60 60 24) #'load-dark-theme)
  (run-at-time "05:00am" (* 60 60 24) #'load-light-theme)
  (run-at-time "05:00pm" (* 60 60 24) #'load-dark-theme))

;; This must be loaded after themes.
(use-package mood-line
  :config (mood-line-mode))

(use-package smart-mode-line :disabled
  :config
  ;; These aren't set with `custom' because their `:set' functions
  ;; cause the theme to be applied multiple times.
  (setq sml/theme 'respectful ; Better than `automatic' which is very plain.
        sml/no-confirm-load-theme t
        sml/name-width 32
        sml/shorten-modes nil
        sml/replacer-regexp-list nil)
  (sml/setup))

;;; Internal Emacs Configuration:

(customize-set-variable 'gc-cons-threshold (* 100 1024 1024))
(customize-set-variable 'read-process-output-max (* 1024 1024))

;; Fix annoyances.
(defalias 'yes-or-no-p 'y-or-n-p)
(customize-set-variable 'minibuffer-message-timeout 0.5)
(customize-set-variable 'set-mark-command-repeat-pop t)
(customize-set-variable 'delete-by-moving-to-trash t)
(customize-set-variable 'create-lockfiles nil)
(customize-set-variable 'ring-bell-function 'ignore)
(customize-set-variable 'visible-bell t)
(customize-set-variable 'inhibit-startup-screen t)
(set-variable 'disabled-command-function nil)

(use-feature help
  :config
  (bind-key "C-h L" #'find-library)
  :custom (help-window-select t))

;; Save Emacs sessions
(use-feature desktop :disabled
  :config
  (add-to-list 'desktop-globals-to-save 'kill-ring)
  (desktop-save-mode)
  ;; TODO: This is a bug in `desktop.el’. Since `desktop-kill’ can be
  ;; interactive (asks if the desktop should be saved), it should not
  ;; be in `kill-emacs-hook’ and having it there prevents Emacs from
  ;; being killed externally by the kill signal, since Emacs is
  ;; sitting elsewhere waiting for input.
  (remove-hook 'kill-emacs-hook 'desktop-kill))

;; Simple is Emacs's built-in miscellaneous package.
(use-feature simple
  :config
  (bind-keys
   ([remap just-one-space] . cycle-spacing)
   ([remap upcase-word] . upcase-dwim)
   ([remap downcase-word] . downcase-dwim)
   ([remap capitalize-word] . capitalize-dwim)
   ([remap zap-to-char] . zap-up-to-char))
  (column-number-mode)
  :custom
  ;; TODO: Maybe set `suggest-key-bindings' to `nil'.
  (save-interprogram-paste-before-kill t)
  (kill-do-not-save-duplicates t)
  (kill-whole-line t)
  (shift-select-mode nil "Don’t activate mark with shift.")
  (select-active-regions nil "Don’t set primary selection.")
  ;; Share clipboard when `DISPLAY’ is set.
  (select-enable-clipboard (getenv "DISPLAY") "Don’t share clipboard with yank")
  (visual-line-fringe-indicators '(nil right-curly-arrow)))

(use-feature files
  :custom
  (confirm-kill-emacs 'y-or-n-p)
  (confirm-nonexistent-file-or-buffer t)
  (save-abbrevs 'silently)
  (auto-save-no-message t)
  (require-final-newline t)
  (backup-by-copying t)
  (delete-old-versions t)
  (version-control t)
  (large-file-warning-threshold (* 20 1000 1000) "20 megabytes.")
  :config
  (auto-save-visited-mode) ; Also save files directly when idle.
  (add-to-list
   'backup-directory-alist
   `(,tramp-file-name-regexp . ,(no-littering-expand-var-file-name "tramp/backup/"))))

;;; Language Modes:

(add-args-to-list
 'auto-mode-alist '(("\\.ino\\'"  . c-mode)
                    ("\\.cfg\\'"  . conf-mode)
                    ("\\.vcsh\\'" . conf-mode)
                    ("\\.zsh\\'"  . sh-mode)))

(use-package apt-sources-list)

(use-package bazel-mode :disabled)

;; OCaml
(use-package tuareg
  :defines tuareg-mode-map
  :bind (:map tuareg-mode-map ([remap indent-region] . ocamlformat)))

(use-package ocamlformat :disabled
  ;; TODO: May want to limit this to certain files.
  :hook (tuareg-mode . (lambda ()
                         (add-hook 'before-save-hook #'ocamlformat-before-save nil 't)))
  :custom (ocamlformat-show-errors nil))

(use-package dune :disabled)

(use-package utop :disabled) ; OCaml shell

(use-package merlin
  :defines merlin-mode-map
  :hook (tuareg-mode . merlin-mode)
  :bind (:map merlin-mode-map
              ;; TODO: Maybe map phrases to paragraphs.
              ([remap xref-find-definitions] . merlin-locate)
              ([remap xref-pop-marker-stack] . merlin-pop-stack)))

(use-package merlin-eldoc
  :hook (tuareg-mode . merlin-eldoc-setup))
;; OCaml setup ends here

(use-package cmake-mode
  :defines cmake-mode-map
  :bind (:map cmake-mode-map
              ([remap xref-find-definitions] . cmake-help-command)))

(use-package csharp-mode)

(use-package dockerfile-mode)

(use-package fish-mode :disabled)

(use-package git-modes)

(use-package groovy-mode :disabled)

(use-package markdown-mode
  :custom (markdown-fontify-code-blocks-natively t)
  :config
  (add-hook 'markdown-mode-hook #'turn-on-auto-fill)
  (add-hook 'markdown-mode-hook (lambda () (set-fill-column 80))))

;; Enables `markdown-edit-code-block'.
(use-package edit-indirect)

(use-package nginx-mode)

(use-package powershell)

(use-package protobuf-mode :disabled)

(use-package puppet-mode :disabled)

(use-package pyvenv
  :hook (python-mode . pyvenv-tracking-mode))

(use-package rust-mode
  :custom (rust-format-on-save t))

(use-package cargo)

(use-package systemd)

(use-package ssh-config-mode)

(use-package toml-mode)

(use-package yaml-mode)

;;; Finish Loading:

(server-start)
(provide 'init)

;;; init.el ends here
