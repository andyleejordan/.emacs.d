;;; early-init.el --- Package setup                  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023 Andy Jordan

;; Author: Andy Jordan <2226434+andyleejordan@users.noreply.github.com>

;; This file is not part of GNU Emacs.

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

;; This is `early-init.el' used by Emacs 27+ for package configuration.

;;; Code:

;; Avoiding bugs.
(setq load-prefer-newer t)

;; Improved TLS Security.
(with-eval-after-load 'gnutls
  (custom-set-variables
   '(gnutls-verify-error t)
   '(gnutls-min-prime-bits 3072)))

;; Package setup.
(eval-when-compile
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("org" . "https://orgmode.org/elpa/") t))

;; Frame parameters.
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Layout-Parameters.html
(when (getenv "DISPLAY") ; `display-graphic-p' is not defined this early
  (setq default-frame-alist
        '(;; Set default frame size and position.
          (height . 50) (width . 100)
          (top . 0) (left . 0)
          ;; Disable archaic bars.
          (vertical-scroll-bars . nil)
          (horizontal-scroll-bars . nil)
          (tool-bar-lines . 0)
          ;; Fix invisible buffer content when X is tunneled
          ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25474
          (inhibit-double-buffering . t))))

(provide 'early-init)
;;; early-init.el ends here
