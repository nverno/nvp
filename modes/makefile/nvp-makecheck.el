;;; nvp-makecheck.el --- lint/debug makefile -*- lexical-binding: t; -*-

;;; Commentary:
;; - https://github.com/mrtazz/checkmake
;; - make --warn-undefined-variables
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:req 'nvp-makefile 'subrs)
(require 'compile)
(require 'nvp)

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
