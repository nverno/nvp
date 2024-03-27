;;; nvp-hap-lsp.el --- lsp help-at-point -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'nvp-hap)
(nvp:decls :p (lsp) :f (lsp-help-mode))

(defsubst nvp-hap-lsp-server-id ()
  (lsp--workspace-server-id (car-safe (lsp-workspaces))))

(defmacro nvp-with-hap-lsp-buffer (&optional bufname &rest body)
  (declare (indent 1))
  `(let ((lsp-help-buf-name ,(or bufname "*lsp-help*")))
     (with-current-buffer (get-buffer-create lsp-help-buf-name)
       (let ((delay-mode-hooks t))
         (lsp-help-mode)
         (prog1 (with-help-window lsp-help-buf-name
                  ,@body)
           (run-mode-hooks))))))

(defun nvp-hap-lsp-thingatpt ()
  (let ((contents (-some->> (lsp--text-document-position-params)
                    (lsp--make-request "textDocument/hover")
                    (lsp--send-request)
                    (lsp:hover-contents))))
    (and contents (not (equal "" contents))
         (cond
          ((hash-table-p contents)
           (not (or (hash-table-empty-p contents)
                    (equal "" (gethash "value" contents)))))
          ((vectorp contents)
           (> (length contents) 0))
          (t t))
         contents)))

(cl-defgeneric nvp-hap-lsp-search-remote (_server-id &optional _arg)
  "Search remote docs for ARG or thing-at-point according to SERVER-ID."
  (user-error "unimplemented"))

;;;###autoload
(defun nvp-hap-lsp (command &optional arg &rest _args)
  (cl-case command
    (thingatpt (nvp-hap-lsp-thingatpt))
    (doc-string (string-trim-right (lsp--render-on-hover-content arg t)))
    (doc-buffer
     (let ((display-buffer-overriding-action
            '(nil . ((inhibit-switch-frame . t)))))
       (nvp-with-hap-lsp-buffer nil
         (insert (string-trim-right (lsp--render-on-hover-content arg t)))
         (list (current-buffer) (point-min) nil))))
    (search-remote (nvp-hap-lsp-search-remote (nvp-hap-lsp-server-id) arg))))

(provide 'nvp-hap-lsp)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-hap-lsp.el ends here
