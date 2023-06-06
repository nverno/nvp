;;; nvp-makecheck.el --- lint makefile -*- lexical-binding: t; -*-

;;; Commentary:
;; - https://github.com/mrtazz/checkmake
;; - make --warn-undefined-variables
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'compile)
(require 'nvp)

;;;###autoload
(defun nvp-makefile-check (&optional targets)
  "Dry run makefile TARGETS to report undefined variables in compilation buffer."
  (interactive
   (list
    (mapconcat 'identity (nvp-makefile-completing-read (buffer-file-name)) " ")))
  (let ((compilation-error-regexp-alist '(makefile))
        (compilation-error-regexp-alist-alist
         '(makefile "\\([^:]+\\):\\([0-9]+\\)" 1 2)))
    (compilation-start
     (concat "make -n --warn-undefined-variables -f " (buffer-file-name) " " targets))))

(provide 'nvp-makecheck)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-makecheck.el ends here
