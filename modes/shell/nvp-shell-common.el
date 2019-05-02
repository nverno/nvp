;;; nvp-shell-common.el --- shared utils/vars -*- lexical-binding: t; -*-

;;; Commentary:
;; stuff required by numerous modes
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(nvp-decls)

;; dont expand when prefixed by [-/_.]
(defvar nvp-shell-abbrev-re "\\(\\_<[_:\\.A-Za-z0-9/-]+\\)")

;; look for an active interactive shell process
(defun nvp-shell-get-process (&optional proc-name buffer-name)
  (cl-loop for proc in (process-list)
     when (and (process-live-p proc)
               (cond
                (proc-name (string= (process-name proc) proc-name))
                (buffer-name (string= (buffer-name (process-buffer proc))
                                      buffer-name))
                (t (process-command proc)
                   (cl-find "-i" (process-command proc) :test 'string=))))
     return proc))

(provide 'nvp-shell-common)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-shell-common.el ends here
