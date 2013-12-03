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
                      ido-ubiquitous
		      linum-relative
                      magit
                      markdown-mode
		      php-mode
                      projectile
                      puppet-mode
                      smartparens
                      smex
                      smooth-scroll
		      solarized-theme
                      undo-tree
                      virtualenvwrapper))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;; ace-jump-mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

;;; auto-complete
(require 'auto-complete)
(global-auto-complete-mode t)

;;; activate expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;; browse-kill-ring
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;;; activate flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; dash-at-point
(define-key global-map (kbd "C-c d") 'dash-at-point)

;;; ein
(require 'ein)
(setq ein:use-auto-complete t)

;;; pull in shell path
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;; require ido-ubiquitous
(require 'ido)
(require 'ido-ubiquitous)

;;; ido-mode
(ido-mode t)

;;; flx-ido
(require 'flx-ido)
(flx-ido-mode t)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;;; linum-relative
(require 'linum-relative)

;;; magit
(define-key global-map (kbd "C-c C-g") 'magit-status)

;;; org-mode
(setq org-agenda-files '("~/.org"))
(define-key global-map (kbd "C-c a") 'org-agenda)
(setq org-entities-user '(("join" "\\Join" nil "&#9285;" "" "" "⋈")
			  ("reals" "\\mathbb{R}" t "&#8477;" "" "" "ℝ")
			  ("ints" "\\mathbb{Z}" t "&#8484;" "" "" "ℤ")
			  ("models" "\\models" nil "&#8872;" "" "" "⊧")))

;;; activate projectile
(require 'projectile)
(projectile-global-mode)

;;; activate smartparens
(smartparens-global-mode t)
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
(sp-local-pair 'org-mode "'" nil :actions nil)
(sp-local-pair 'text-mode "'" nil :actions nil)
(sp-local-pair 'git-commit-mode "'" nil :actions nil)

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

;;; emacs configurations

;;; disable bell function
(setq ring-bell-function 'ignore)

;;; eval-buffer
(global-set-key (kbd "C-c C-x") 'eval-buffer)

;;; set auto revert of buffers if file is changed externally
(global-auto-revert-mode t)

;;; backups
(setq backup-by-copying t)

(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

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
