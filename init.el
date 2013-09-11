;;; init --- Andrew Schwartzmeyer's Emacs init file

;;; Commentary:
;;; Fully customized Emacs configurations

;;; Code:

(add-to-list 'load-path "~/.emacs.d/")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(ace-jump-mode
                      auto-complete
                      better-defaults
                      color-theme-solarized
                      ido-ubiquitous
                      expand-region
                      flycheck
                      linum
                      magit
                      markdown-mode
                      paredit
                      projectile
                      puppet-mode
                      smartparens
                      smex
                      smooth-scrolling
                      undo-tree
                      virtualenvwrapper))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;; ace-jump-mode
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;;; auto-complete
(require 'auto-complete)
(global-auto-complete-mode t)

;;; require better-defaults
(require 'better-defaults)

;;; require ido-ubiquitous
(require 'ido)
(require 'ido-ubiquitous)

;;; activate expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;; activate flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; eval-buffer
(global-set-key (kbd "C-c x") 'eval-buffer)

;;; line numbers
(require 'linum)
(global-linum-mode t)

;;; magit
(define-key global-map (kbd "C-c g") 'magit-status)

;;; org-mode
(setq org-agenda-files '("~/org/personal"
                         "~/org/school"))
(define-key global-map (kbd "C-c a") 'org-agenda)

;;; activate projectile
(require 'projectile)
(projectile-global-mode)

;;; use puppet-mode for *.pp files
(add-to-list 'auto-mode-alist '("\\.pp\\'" . puppet-mode))

;;; activate smartparens
(smartparens-global-mode t)
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)

;;; setup smex bindings
(require 'smex)
(setq smex-save-file (expand-file-name ".smex-items" "~/.emacs.d/"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;; scrolling
(require 'smooth-scrolling)

;;; activate solarized-dark theme
(load-theme 'solarized-dark t)

;;; undo-tree
(global-undo-tree-mode)

;;; setup virtualenvwrapper
(require 'virtualenvwrapper)
(setq venv-location "~/.virtualenvs/")

;;; personal functions

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

;;; emacs configurations

;;; disable bell function
(setq ring-bell-function 'ignore)

;;; exec-path
(add-to-list 'exec-path "/usr/local/bin")

;;; set auto revert of buffers if file is changed externally
(global-auto-revert-mode t)

;;; backups
(setq backup-by-copying t)

(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; blink cursor
(blink-cursor-mode t)

;;; flyspell
(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))

;;; visual-line-mode
(global-visual-line-mode t)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;;; whitespace
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))

;;; start server
(server-start)

;;; provide init package
(provide 'init)

;;; init.el ends here
