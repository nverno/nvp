;;; nvp-hap-lsp.el --- lsp help-at-point -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'nvp-hap)
(nvp:decls :f (lsp--text-document-position-params
               lsp-describe-thing-at-point
               lsp--make-request lsp--send-request lsp:hover-contents
               lsp--render-on-hover-content))

;;;###autoload
(defun nvp-hap-lsp (command &optional arg &rest _args)
  (cl-case command
    (thingatpt (let ((contents (-some->> (lsp--text-document-position-params)
                                 (lsp--make-request "textDocument/hover")
                                 (lsp--send-request)
                                 (lsp:hover-contents))))
                 (and contents (not (equal "" contents)) contents)))
    (doc-string (string-trim-right (lsp--render-on-hover-content arg t)))
    (doc-buffer
     (let ((display-buffer-overriding-action
            '(nil . ((inhibit-switch-frame . t))))
           (lsp-help-buf-name "*lsp-help*"))
       (with-current-buffer (get-buffer-create lsp-help-buf-name)
         (insert (string-trim-right (lsp--render-on-hover-content arg t)))
         (list (current-buffer) (point-at-bol) nil))))))

(provide 'nvp-hap-lsp)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-hap-lsp.el ends here
