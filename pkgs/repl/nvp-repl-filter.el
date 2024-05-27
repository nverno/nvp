;;; nvp-repl-filter.el --- Handle additional repl commands -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls)


(defvar-local nvp-repl--input-handlers '()
  "List of cons cells mapping commands to handlers, \\='((cmd . handler)).")

(defun nvp-repl--input-filter (str &optional proc)
  "Filter additional repl commands from STR and apply handlers."
  (if-let* ((handler (assoc-default "?" nvp-repl--input-handlers))
            (help-str (and (string-match "^ *\\? *\\(.+\\)" str)
                           (match-string 1 str))))
      (cond ((functionp handler)
             (let ((res (funcall handler help-str proc)))
               ;; If handler returns 't, delete input and send newline
               (if (eq t res)
                   (progn (and (derived-mode-p 'comint-mode)
                               (comint-delete-input))
                          "\n")
                 res)))
            ((stringp handler)
             (format handler help-str))
            (t help-str))
    str))

(defun nvp-repl@input-sender (orig-fn proc str)
  "Advise local input sender, filtering out and handling any special
 commands."
  (--when-let (nvp-repl--input-filter str proc)
    (funcall orig-fn proc it)))

;;;###autoload
(defun nvp-repl-setup-input-filter (handlers &optional sender-fn)
  "Setup input filtering for added repl commands."
  (when (derived-mode-p 'comint-mode)
    (or sender-fn (setq sender-fn 'comint-input-sender))
    (or comint-input-history-ignore
        (and (assoc-string "?" handlers)
             (setq comint-input-history-ignore (rx bol (* white) "?")))))
  (setq-local nvp-repl--input-handlers (if (functionp handlers)
                                           (funcall handlers)
                                         handlers))
  (when sender-fn
    (add-function :around (local sender-fn) #'nvp-repl@input-sender)))

(provide 'nvp-repl-filter)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-repl-filter.el ends here
