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
                      color-theme-solarized
                      elfeed
                      expand-region
                      flycheck
                      ido-ubiquitous
                      linum
                      magit
                      markdown-mode
                      o-blog
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

;;; activate expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;; set-up elfeed
(global-set-key (kbd "C-x w") 'elfeed)
(setq elfeed-feeds
      '("http://nullprogram.com/feeds/"
        "http://www.terminally-incoherent.com/blog/feed/"
        "http://emacs-fu.blogspot.com/feeds/posts/default/-/new"
        "http://linuxmantra.com/feed"
        "http://pauls-techno-blog.tumblr.com/rss"
        "http://feeds.feedburner.com/technovelty"
        "http://blog.recursion.es/feed/"
        "http://www.windytan.com/feeds/posts/default"
        "http://blog.theincredibleholk.org/atom.xml"
        "http://feeds.feedburner.com/stevelosh"
        "http://www.rdegges.com/feeds/atom.xml"
        "http://feeds.feedburner.com/ThreeProgrammersWalkedIntoABar"
        "http://www.pythondiary.com/blog.xml"
        "http://feeds.feedburner.com/blogspot/MKuf"
        "http://feeds.feedburner.com/redditblog"
        "http://feeds.feedburner.com/ItsNotWorkWhenYouLoveIt"
        "http://www.hackthings.com/feed/"
        "https://github.com/blog/subscribe"
        "https://www.simple.com/blog/"
        "http://feeds.feedburner.com/oatmealfeed"
        "http://xkcd.com/atom.xml"
        "http://feeds.feedburner.com/damninteresting/all"
        "http://feeds.feedburner.com/Explosm"
        "http://what-if.xkcd.com/feed.atom"
        "https://www.schneier.com/blog/atom.xml"))

;;; activate flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; require ido-ubiquitous
(require 'ido)
(require 'ido-ubiquitous)

;;; line numbers
(require 'linum)
(global-linum-mode t)

;;; magit
(define-key global-map (kbd "C-c g") 'magit-status)

;;; org-mode
(setq org-agenda-files '("~/Documents/org/personal"
                         "~/Documents/org/school"))
(define-key global-map (kbd "C-c a") 'org-agenda)
(setq org-entities-user '(("join" "\join" nil "&#9285;" "⋈" "" "⋈")))

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

;;; remap command to meta (while in emacs)
(setq mac-command-modifier 'meta)
(global-set-key (kbd "M-`") 'other-frame)

;;; disable bell function
(setq ring-bell-function 'ignore)

;;; eval-buffer
(global-set-key (kbd "C-c x") 'eval-buffer)

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

;;; start server
(server-start)

;;; provide init package
(provide 'init)

;;; below stolen from better-defaults

;;; disable toolbar and scrollbar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;;; ido-mode
(ido-mode t)
(setq ido-enable-flex-matching t)

;;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;; saveplace

(require 'saveplace)
(setq-default save-place t)

;;; better keys
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; isearch
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;;; matching parentheses
(show-paren-mode 1)

;;; init.el ends here
