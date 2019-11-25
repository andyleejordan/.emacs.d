;;; edl-mode.el --- Support for the Enclave Definition Language -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Andrew Schwartzmeyer

;; Author: Andrew Schwartzmeyer <andrew@schwartzmeyer.com>
;; Created: 22 Jan 2019
;; Keywords: languages, c

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

;; This is the start of a major mode to support Intel's Enclave
;; Definition Language. Currently, it just pretends it is a C file.

;;; Code:

(define-derived-mode edl-mode c-mode "EDL"
  "Major mode for the Enclave Definition Language.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.edl\\'" . edl-mode))

(provide 'edl-mode)
;;; edl-mode.el ends here
