;;; test.el --- Simple test file                     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Andy Schwartzmeyer

;; Author: Andy Schwartzmeyer <andrew@schwartzmeyer.com>
;; Keywords: lisp

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

;; Run with `emacs -Q -l test.el'.

;;; Code:

;; Typical `package.el' setup with Melpa.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

;; Insert repro code here:

;; E.g. `(package-install 'foo)'

(provide 'test)
;;; test.el ends here
