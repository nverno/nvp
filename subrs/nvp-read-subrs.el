;;; nvp-read-subrs.el ---  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(require 'nvp-macro)
(require 'abbrev)
(require 'nvp)

;; list filenames relative to ROOT matching REGEXP
(defsubst nvp-read--relative-files (&optional root regexp)
  (let ((default-directory (or root default-directory)))
    (mapcar (lambda (f) (file-relative-name f))
            (directory-files-recursively root (or regexp "")))))

;; just MODE's name minus the "-mode"
(defsubst nvp-read--mode-name (&optional mode)
  (setq mode (nvp-as-string (or mode major-mode)))
  (string-remove-suffix "-mode" mode))

(defmacro nvp-read:with-fallback (&rest body)
  "Do BODY with custom `ido-fallback-command'."
  (declare (indent defun))
  `(nvp-with-letf 'ido-fallback-command 'nvp-read--ido-fallback
     ,@body))

(defmacro nvp-read:default (default &rest body)
  (macroexp-let2 nil def default
    `(if (eq ,def :none) nil
       (or ,def ,@body))))

(provide 'nvp-read-subrs)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-read-subrs.el ends here
