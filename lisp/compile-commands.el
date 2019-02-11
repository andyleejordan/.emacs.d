;;; compile-commands.el --- Wrappers around `compile' for my projects.  -*- lexical-binding: t; -*-

;;; Commentary:

;; This is part of Andrew Schwartzmeyer's Emacs customizations. See
;; `init.el' for license etc.

;;; Code:

(defun build-open-enclave (arg cmd dir)
  "Build Open Enclave in DIR.
With ARG, clean first. CMD is one of `configure', `compile', or `test'."
  (interactive (list current-prefix-arg
                     (intern (completing-read "Command? " (list 'clean 'configure 'compile 'test) nil t))
                     (let ((root (expand-file-name "build" (cdr (project-current)))))
                       (expand-file-name (read-directory-name "Build directory? " root) root))))
  (when (or arg (eq cmd 'clean)) (delete-directory dir t))
  (make-directory dir t)
  (when (eq cmd 'clean) (return))
  (let ((compile-command
         ;; TODO: Use a join function join these with ` && ' instead
         ;; of this terrible `concat'.
         (combine-and-quote-strings
          (remove nil `(,(concat "cd " (abbreviate-file-name dir)) ; TODO: Make this work with Tramp.
                        ,(when (member cmd (list 'configure 'compile 'test))
                           (let ((root (cdr (project-current)))
                                 ;; TODO: Check for SGX support.
                                 (sgx nil))
                             (format "cmake %s -GNinja -DUSE_LIBSGX=%s" root (if sgx "ON" "OFF"))))
                        ,(when (member cmd (list 'compile 'test)) "ninja -v")
                        ,(when (eq cmd 'test)
                           (let ((re (completing-read "Regex? " (list "^edger8r_" "signedness"))))
                             (concat "ctest -V -R " re)))))
          " && ")))
    (call-interactively 'compile)))

(provide 'compile-commands)
;;; compile-commands.el ends here