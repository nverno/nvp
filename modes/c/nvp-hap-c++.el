;;; nvp-hap-c++.el --- c++ help-at-point -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'nvp-hap)
(require 'nvp-hap-man)
(nvp:decls)

;;; TODO: use man pages from stdman
;;;###autoload
(defun nvp-hap-c++ (command &optional arg &rest _args)
  (cl-case command
    (thingatpt (nvp-hap-man-thingatpt))
    (doc-string)
    (doc-buffer
     (unless (stringp arg) (setq arg (symbol-name arg))))))

(provide 'nvp-hap-c++)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-hap-c++.el ends here
