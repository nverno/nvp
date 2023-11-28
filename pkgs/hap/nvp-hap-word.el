;;; nvp-hap-word.el --- Dictionary hap backend -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls :p (dictionary))
(nvp:auto "dictionary" dictionary-new-search)

(declare-function nvp-hap---dict-hook "")
(defun nvp-hap--dictionary-post-hook (callback)
  (fset 'nvp-hap---dict-hook
        `(lambda ()
           (remove-hook 'dictionary-post-buffer-hook #'nvp-hap---dict-hook)
           (funcall ',callback (current-buffer)))))

;;;###autoload
(defun nvp-hap-word (command &optional arg &rest _args)
  (cl-case command
    (thingatpt (current-word))
    (doc-buffer
     (save-window-excursion
       (condition-case err
           (let* ((buf)
                  (callback (lambda (dict-buf) (setq buf dict-buf))))
             (nvp-hap--dictionary-post-hook callback)
             (add-hook 'dictionary-post-buffer-hook #'nvp-hap---dict-hook)
             (while-no-input
               (dictionary-new-search (cons arg dictionary-default-dictionary))
               (while (not buf)
                 (sit-for 0.1)))
             (and buf (list buf nil)))
         (error
          (message (error-message-string err))
          (remove-hook 'dictionary-post-buffer-hook #'nvp-hap---dict-hook)
          nil))))))

(provide 'nvp-hap-word)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-hap-word.el ends here
