;;; compile-commands.el --- Wrappers around `compile' for my projects.  -*- lexical-binding: t; -*-

;;; Commentary:

;; This is part of Andrew Schwartzmeyer's Emacs customizations. See
;; `init.el' for license etc.

;;; Code:

(defun project-relative (dir) (file-relative-name (cdr (project-current)) dir))

(defun oe-build (arg cmd dir)
  "Build Open Enclave in DIR.
With ARG, clean first. CMD is one of `clean', `configure',
`compile', `debug', or `test'."
  (interactive (list current-prefix-arg
                     (intern (completing-read "Command? " '(clean configure compile debug test) nil t))
                     (let ((root (expand-file-name "build" (cdr (project-current)))))
                       (expand-file-name (read-directory-name "Build directory? " root) root))))
  (when (or arg (eq cmd 'clean)) (delete-directory dir t))
  (make-directory dir t)
  (cond ((eq cmd 'clean) nil)
        ((member cmd '(configure compile test)) (oe-compile cmd dir))
        ((eq cmd 'debug) (oe-gdb dir))))

(defun oe-compile (cmd dir)
  "Handles configure/compile/test CMDs in DIR, which all use `compile'."
  (let ((default-directory dir) ; Using this instead of `cd' works with Tramp.
        (compile-command
         (combine-and-quote-strings
          (remove nil `(,(let ((root (project-relative dir))
                               (sgx (file-exists-p (concat (file-remote-p default-directory) "/dev/sgx"))))
                           (format "cmake %s -GNinja -DUSE_LIBSGX=%s -DENABLE_REFMAN=%s"
                                   root
                                   (if sgx "ON" "OFF")
                                   (if (executable-find "doxygen") "ON" "OFF")))
                        ,(when (member cmd '(compile test)) "ninja -v")
                        ,(when (eq cmd 'test)
                           ;; TODO: `defvar' a list of test expressions. Add history. Allow no regex.
                           (let ((re (completing-read "Regex? " '("oeedger8r" "^edger8r_" "signedness"))))
                             (concat "ctest -V -R " re))))) ; TODO: Set OE_SIMULATION based on `sgx'
          " && ")))
    (call-interactively 'compile)))

(defun oe-gdb (dir)
  "Launch oe-gdb in DIR."
  (let* ((default-directory dir)
         (host (completing-read "Host? " '("tests/oeedger8r/host/edl_host") nil t))
         (enclave (completing-read "Enclave? " '("../enc/edl_enc") nil t))
         (command (concat dir "/output/bin/oegdb -i=mi --args " host " " enclave)))
    (gdb command)))

(provide 'compile-commands)
;;; compile-commands.el ends here
