;;; solarized-andy.el --- Custom Solarized child themes  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Andrew Schwartzmeyer

;; Author: Andrew Schwartzmeyer <andrew@schwartzmeyer.com>
;; Keywords: faces

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A pair of child themes derived from Solarized, because I have a lot
;; more faces that need defining, and want to use the built-in names
;; (especially for the base colors which change with dark vs light).

;;; Code:
(require 'solarized)
(eval-when-compile
  (require 'solarized-palettes))

(defconst solarized-subtheme
  '((custom-theme-set-faces
     theme-name
     ;; Tabs
     `(tab-bar
       ((,class (:foreground ,base0 :background ,base02))))
     `(tab-bar-tab
       ((,class ( ; Matches the buffer background
                 :inherit tab-bar
                 :weight bold
                 :background ,base03))))
     `(tab-bar-tab-inactive
       ((,class (:inherit tab-bar))))
     ;; Mode-line
     `(mode-line
       ((,class ( ; No underline
                 :foreground ,s-mode-line-fg
                 :background ,s-mode-line-bg))))
     `(mode-line-inactive
       ((,class ( ; Keep underline
                 :inherit mode-line
                 :foreground ,s-mode-line-inactive-fg
                 :background ,s-mode-line-inactive-bg
                 :underline ,s-mode-line-underline))))
     ;; Icomplete
     `(icomplete-first-match
       ((,class (:weight bold :foreground ,green))))
     ;; Completions
     `(completions-first-difference
       ((,class (:foreground ,magenta))))
     `(completions-common-part
       ((,class (:foreground ,yellow))))
     `(completions-annotations
       ((,class (:foreground ,cyan))))
     ;; Orderless
     `(orderless-match-face-0
       ((,class (:weight bold :foreground ,magenta))))
     `(orderless-match-face-1
       ((,class (:weight bold :foreground ,yellow))))
     `(orderless-match-face-2
       ((,class (:weight bold :foreground ,blue))))
     `(orderless-match-face-3
       ((,class (:weight bold :foreground ,cyan))))
     ;; Dired+
     `(diredp-autofile-name
       ((,class (:inherit shadow))))
     `(diredp-compressed-file-name
       ((,class (:inherit default))))
     `(diredp-compressed-file-suffix
       ((,class (:foreground ,cyan))))
     `(diredp-date-time
       ((,class (:inherit shadow))))
     `(diredp-deletion ; (D) flag
       ((,class (:foreground ,orange))))
     `(diredp-deletion-file-name
       ((,class (:foreground ,red :weight bold))))
     `(diredp-dir-heading
       ((,class (:inherit dired-directory :weight bold))))
     `(diredp-dir-name
       ((,class (:inherit dired-directory))))
     `(diredp-dir-priv ; ‘d’ flag
       ((,class (:foreground ,blue))))
     `(diredp-exec-priv ; ‘x’ flag
       ((,class (:inherit default))))
     `(diredp-executable-tag ; ‘*’ flag after name
       ((,class (:foreground ,magenta))))
     `(diredp-file-name
       ((,class (:foreground ,base1))))
     `(diredp-file-suffix ; extension
       ((,class (:foreground ,yellow))))
     `(diredp-flag-mark ; ‘*’ flag
       ((,class (:foreground ,yellow))))
     `(diredp-flag-mark-line
       ((,class (:foreground ,green :weight bold))))
     `(diredp-ignored-file-name
       ((,class (:inherit dired-ignored))))
     `(diredp-link-priv ; ‘l’ flag
       ((,class (:inherit dired-symlink))))
     `(diredp-mode-line-marked
       ((,class (:foreground ,violet))))
     `(diredp-mode-line-flagged
       ((,class (:foreground ,red))))
     `(diredp-no-priv ; ‘-’ flag
       ((,class (:inherit shadow))))
     `(diredp-number ; size and file counts
       ((,class (:inherit shadow))))
     `(diredp-omit-file-name
       ((,class (:foreground ,orange))))
     `(diredp-other-priv ; ‘l,s,S,t,T’ flags
       ((,class (:foreground ,cyan))))
     `(diredp-rare-priv ; ‘b,c,s,m,p,S’ flags
       ((,class (:foreground ,cyan))))
     `(diredp-read-priv ; ‘r’ flag
       ((,class (:inherit default))))
     `(diredp-symlink ; ‘l’ flag
       ((,class (:inherit dired-symlink))))
     `(diredp-tagged-autofile-name
       ((,class (:inherit shadow))))
     `(diredp-write-priv ; ‘w’ flag
       ((,class (:inherit default))))
     ;; Help+
     `(describe-variable-value
       ((,class (:foreground ,green))))
     ;; Isearch+
     `(isearchp-multi
       ((,class (:foreground ,violet))))
     `(isearchp-overwrapped
       ((,class (:overline  ,magenta))))
     `(isearchp-regexp
       ((,class (:foreground ,red))))
     `(isearchp-word
       ((,class (:foreground ,green))))
     `(isearchp-wrapped
       ((,class (:overline ,blue))))
     ;; Isearch+ regexp groups
     `(isearchp-regexp-level-1
       ((,class (:foreground ,base03 :background ,magenta))))
     `(isearchp-regexp-level-2
       ((,class (:foreground ,base03 :background ,violet))))
     `(isearchp-regexp-level-3
       ((,class (:foreground ,base03 :background ,blue))))
     `(isearchp-regexp-level-4
       ((,class (:foreground ,base03 :background ,cyan))))
     `(isearchp-regexp-level-5
       ((,class (:foreground ,base03 :background ,green))))
     `(isearchp-regexp-level-6
       ((,class (:foreground ,base03 :background ,red))))
     `(isearchp-regexp-level-7
       ((,class (:foreground ,base03 :background ,orange))))
     `(isearchp-regexp-level-8
       ((,class (:foreground ,base03 :background ,yellow))))
     `(isearchp-lazy-odd-regexp-groups
       ((,class (:background ,cyan)))))))

(deftheme solarized-andy-light "Custom light variant of Solarized.")
(solarized-create-theme-file 'light 'solarized-andy-light
  solarized-light-color-palette-alist solarized-subtheme t)

(deftheme solarized-andy-dark "Custom dark variant of Solarized.")
(solarized-create-theme-file 'dark 'solarized-andy-dark
  solarized-dark-color-palette-alist solarized-subtheme t)

(provide 'solarized-andy)
;;; solarized-andy.el ends here
