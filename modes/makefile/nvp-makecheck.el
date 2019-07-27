;;; nvp-makecheck.el --- lint makefile -*- lexical-binding: t; -*-

;;; Commentary:
;; - https://github.com/mrtazz/checkmake
;; - make --warn-undefined-variables
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'nvp)

;;;###autoload
(defun nvp-makefile-check ()
  (interactive)
  (let ((compilation-error-regexp-alist '(makefile))
        (compilation-error-regexp-alist-alist
         '(makefile "\\([^:]+\\):\\([0-9]+\\)" 1 2)))
    (compilation-start
     (concat "make -n --warn-undefined-variables -f " (buffer-file-name)))))

(provide 'nvp-makecheck)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-makecheck.el ends here
