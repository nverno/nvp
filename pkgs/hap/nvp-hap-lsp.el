;;; nvp-hap-lsp.el --- lsp help-at-point -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'nvp-hap)
(nvp:decls :f (lsp-describe-thing-at-point))

;;;###autoload
(defun nvp-hap-lsp (command &optional art &rest _args)
  (cl-case command
    (thingatpt ())
    (doc-buffer
     (save-window-excursion
       (let ((display-buffer-overriding-action
              '(nil . ((inhibit-switch-frame . t)))))
         (when (cond
                ((fboundp arg) (describe-function arg))
                ((boundp arg) (describe-variable arg))
                (t nil))
           (list (help-buffer) (point-min) nil)))))))

(provide 'nvp-hap-lsp)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-hap-lsp.el ends here
