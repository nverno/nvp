;;; nvp-cmake-abbrev.el --- CMake abbrevs -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-cmake)
(nvp:decls :v (cmake-mode-abbrev-table))

(defvar nvp-cmake-abbrev-syntax-table
  (let ((tab (make-syntax-table)))
    (modify-syntax-entry ?_ "w" tab)
    (modify-syntax-entry ?- "w" tab)
    (modify-syntax-entry ?$ "'" tab)
    tab))

;;;###autoload
(defun nvp-cmake-expand-abbrev ()
  (nvp:with-letf 'forward-word 'forward-word-strictly
    (c-with-syntax-table nvp-cmake-abbrev-syntax-table
      (abbrev--default-expand))))

(defvar nvp-cmake-builtin-variables
  (eval-when-compile
    (process-lines
     (expand-file-name "bin/dump-vars.awk" (nvp:package-root nvp-cmake)))))

;;;###autoload
(defun nvp-cmake-add-abbrevs (&optional prefix)
  "Add abbrevs mapping lowercase cmake properties/variables to uppercase.
With PREFIX, just list them."
  (interactive "P")
  (pcase prefix
    ('(4) (nvp:with-results-buffer :title "Cmake properties/variables"
            (call-process-shell-command
             (expand-file-name "bin/dump-vars.awk" (nvp:package-root nvp-cmake))
             nil (current-buffer))))
    (_ (mapc (lambda (a) (define-abbrev cmake-mode-abbrev-table (downcase a) a))
             nvp-cmake-builtin-variables))))


(provide 'nvp-cmake-abbrev)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-cmake-abbrev.el ends here
