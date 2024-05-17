;;; nvp-scheme-help.el --- scheme help -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'geiser nil t)
(require 'geiser-doc nil t)
(nvp:decls :p (geiser) :f (geiser-eval--get-module))
(nvp:auto "nvp-hap" nvp-hap-thing-at-point)

;;;###autoload
(defun nvp-scheme-help-company-show-doc ()
  "Show the doc for current company selection."
  (interactive)
  (let ((selected (nth company-selection company-candidates)))
    (geiser-doc--external-help geiser-impl--implementation
                               selected (geiser-eval--get-module))))

;;;###autoload
(defun nvp-hap-scheme (command &optional arg &rest _args)
  (cl-case command
    (thingatpt (intern (nvp-hap-thing-at-point arg)))
    (doc-buffer
     (save-window-excursion
       (geiser-doc-symbol arg)
       (list (current-buffer) (point-min))))))

(provide 'nvp-scheme-help)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-scheme-help.el ends here
