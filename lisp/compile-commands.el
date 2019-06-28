;;; compile-commands.el --- Wrappers around `compile' for my projects.  -*- lexical-binding: t; -*-

;;; Commentary:

;; This is part of Andrew Schwartzmeyer's Emacs customizations. See
;; `init.el' for license etc.

;;; Code:

(defun project-relative (dir) (file-relative-name (cdr (project-current)) dir))

(defun oe-build (arg cmd dir)
  "Build Open Enclave in DIR.
With ARG, clean first. CMD is one of `configure', `compile', or `test'."
  (interactive (list current-prefix-arg
                     (intern (completing-read "Command? " (list 'clean 'configure 'compile 'test) nil t))
                     (let ((root (expand-file-name "build" (cdr (project-current)))))
                       (expand-file-name (read-directory-name "Build directory? " root) root))))
  (when (or arg (eq cmd 'clean)) (delete-directory dir t))
  (make-directory dir t)
  (unless (eq cmd 'clean)
    (let ((default-directory dir) ; Using this instead of `cd' works with Tramp.
          (compile-command
           (combine-and-quote-strings
            (remove nil `(,(when (member cmd (list 'configure 'compile 'test))
                             (let ((root (project-relative dir))
                                   (sgx (file-exists-p (concat (file-remote-p default-directory) "/dev/sgx"))))
                               (format "cmake %s -GNinja -DUSE_LIBSGX=%s%s" root (if sgx "ON" "OFF") (unless (executable-find "doxygen") " -DENABLE_REFMAN=OFF"))))
                          ,(when (member cmd (list 'compile 'test)) "ninja -v")
                          ,(when (eq cmd 'test)
                             ;; TODO: `defvar' a list of test expressions. Add history. Allow no regex.
                             (let ((re (completing-read "Regex? " (list "oeedger8r" "^edger8r_" "signedness"))))
                               (concat "ctest -V -R " re))))) ; TODO: Set OE_SIMULATION based on `sgx'
            " && ")))
      (call-interactively 'compile))))

(defun oe-gdb (arg dir host enclave)
  "Launch oe-gdb for HOST and ENCLAVE in DIR. Ignore ARG."
  (interactive (list current-prefix-arg
                     (let ((root (expand-file-name "build" (cdr (project-current)))))
                       (expand-file-name (read-directory-name "Build directory? " root) root))
                     (intern (completing-read "Host? " (list "/tests/oeedger8r/host/edl_host") nil t))
                     (intern (completing-read "Enclave? " (list "/tests/oeedger8r/enc/edl_enc") nil t))))
  (let* ((root (project-relative dir))
         (host (concat dir "tests/oeedger8r/host/edl_host"))
         (enclave (concat dir "tests/oeedger8r/enc/edl_enc"))
         (gud-gdb-command-name (concat dir "output/bin/oegdb -i=mi --args " host " " enclave)))
    (call-interactively 'gdb)))

(provide 'compile-commands)
;;; compile-commands.el ends here
