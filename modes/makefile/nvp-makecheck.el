;;; nvp-makecheck.el --- lint/debug makefile -*- lexical-binding: t; -*-

;;; Commentary:
;; - https://github.com/mrtazz/checkmake
;; - make --warn-undefined-variables
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'compile)
(require 'nvp)

(eval-when-compile
  (defsubst nvp:makefile-read-targets ()
    (mapconcat 'identity (nvp-makefile-completing-read (buffer-file-name)) " "))

  (defmacro nvp:makefile-with-compilation-vars (&rest body)
    `(let ((compilation-error-regexp-alist '(makefile))
           (compilation-error-regexp-alist-alist
            '(makefile "\\([^:]+\\):\\([0-9]+\\)" 1 2)))
       ,@body)))

;;;###autoload
(defun nvp-makefile-check (&optional targets)
  "Dry run makefile TARGETS to report undefined variables in compilation buffer."
  (interactive (list (nvp:makefile-read-targets)))
  (nvp:makefile-with-compilation-vars
   (compilation-start
    (concat "make -n --warn-undefined-variables -f " (buffer-file-name) " " targets))))

(defun nvp-makefile-debug (&optional targets)
  "Run makefile TARGETS with shell debugging output."
  (interactive (list (nvp:makefile-read-targets)))
  (nvp:makefile-with-compilation-vars
   (compilation-start
    (concat
     "cat <<TARGET | make -f - run-debug
include " (buffer-file-name) "
_SHELL := \\$(SHELL)
SHELL = \\$(warning [\\$@])\\$(_SHELL) -x
run-debug: " targets "
TARGET
"))))

(provide 'nvp-makecheck)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-makecheck.el ends here
