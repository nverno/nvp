;;; nvp-c-subrs.el --- compile-time -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'nvp-macro)
(defvar check-abbrev-table)
(defvar cunit-abbrev-table)
(defvar unity-abbrev-table)
(defvar nvp-local-abbrev-table)
(defvar boost-test-abbrev-table)

;; locally set keys in test buffers to run tests
(defmacro nvp:c-test--buffer (type)
  `(progn
     (setq-local local-abbrev-table
                 (symbol-value (intern (concat ,type "-abbrev-table"))))
     (setq-local nvp-local-abbrev-table ,type)
     (nvp:set-local-keymap :use t
       ("C-c C-c" . nvp-c-test-run-unit-test))))

(eval-when-compile (require 'nvp-c-compile))

;; generate function to run unit tests
;; Runs tests in current buffer or FILE if non-nil
(defmacro nvp:c-define-test-runner-fn (name &optional c++ flags libs)
  (declare (indent defun))
  (let ((fn (nvp:as-symbol name)))
    `(progn
       ;; (,'declare-function ,fn "")
       (defun ,fn (save &optional file post-compile)
         "Run tests in current buffer or FILE. Don't throw away executable if KEEP
is non-nil."
         (interactive "P")
         (let* ((default-directory (if file (file-name-directory file)
                                     default-directory))
                (out (nvp-c--out-file file))
                (compile-command
                 (concat
                  (nvp:program ,(if c++ "g++" "gcc")) " " ,flags " "
                  " -o " out " " (or file buffer-file-name)
                  (concat " " ,libs ";")
                  (or save post-compile
                      (concat "./" (file-name-nondirectory out)
                              "; rm " out))))
                (compilation-read-command nil))
           (call-interactively 'compile))))))

(defmacro nvp:c++-test--setup-buffer ()
  `(progn
     (setq-local local-abbrev-table boost-test-abbrev-table)
     (nvp:set-local-keymap :use t
       ("C-c C-c" . nvp-c++-test-run-unit-test))))

(provide 'nvp-c-subrs)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-c-subrs.el ends here
