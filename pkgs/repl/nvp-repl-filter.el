;;; nvp-repl-filter.el --- Handle additional repl commands -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Register new repl commands by adding (cmd . handler) entries to a repl's
;; `:cmd-handlers'.
;;
;; CMD is the new repl command that gets typed in the repl.
;; HANDLER is either a string or function and is used to transform the input
;; before being sent to the repl (and prior to `comint-input-sender').
;;
;; When it's a function, the result is (funcall handler args proc). ARGS is any
;; remaining input after the command. If then handler returns `t', the input is
;; deleted and a newline is sent.
;;
;; When the handler is a string, the result is (format handler args) -
;; essentially defining an alias.
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls)


(defvar-local nvp-repl--input-handlers '()
  "List of cons cells mapping commands to handlers, \\='((cmd . handler)).")

(defvar-local nvp-repl--filter-re nil
  "Local regex to match added repl commands.")


(defun nvp-repl--input-filter (str &optional proc)
  "Filter additional repl commands from STR and apply handlers."
  (if-let* ((cmd (and (string-match nvp-repl--filter-re str)
                      (match-string 1 str)))
            (handler (assoc-default cmd nvp-repl--input-handlers)))
      (let ((args (match-string 2 str)))
        (cond ((functionp handler)
               (let ((res (funcall handler args proc)))
                 ;; If handler returns 't, delete input and send newline
                 (if (eq t res)
                     (progn (and (derived-mode-p 'comint-mode)
                                 (comint-delete-input))
                            "\n")
                   res)))
              ((stringp handler)
               (format handler args))
              (t args)))
    str))

(defun nvp-repl@input-sender (orig-fn proc str)
  "Advise local input sender, filtering out and handling any special
 commands."
  (--when-let (nvp-repl--input-filter str proc)
    (funcall orig-fn proc it)))

(defun nvp-repl--make-filter-re (handlers)
  "Make repl command matcher for HANDLERS."
  (when handlers
    (cl-flet ((join (cmds &optional after-re)
                (concat (mapconcat (lambda (c) (format "\\(?:%s\\)" c)) cmds "\\|")
                        after-re)))
      (let (words syms other)
        (dolist (cmd (mapcar #'car handlers))
          (push cmd (cond ((string-match-p "\\s.\\'" cmd) other)
                          ((string-match-p "\\s_\\'" cmd) syms)
                          ((string-match-p "\\w\\'" cmd) words)
                          (t other))))
        (concat
         "\\`\\s-*\\("
         (mapconcat 'identity (delq nil (list (and syms (join syms "\\_>"))
                                              (and words (join words "\\b"))
                                              (and other (join other))))
                    "\\|")
         "\\)\\s-*\\(.+\\)?")))))


;;;###autoload
(defun nvp-repl-setup-input-filter (handlers &optional sender-fn)
  "Setup input filtering for added repl commands."
  (when handlers
    (setq-local nvp-repl--input-handlers (if (functionp handlers)
                                             (funcall handlers)
                                           handlers))
    (setq-local nvp-repl--filter-re (nvp-repl--make-filter-re handlers)))
  (when (derived-mode-p 'comint-mode)
    (or sender-fn (setq sender-fn 'comint-input-sender))
    (or comint-input-history-ignore
        (null nvp-repl--filter-re)
        (setq comint-input-history-ignore nvp-repl--filter-re)))
  (when sender-fn
    (add-function :around (local sender-fn) #'nvp-repl@input-sender)))

(provide 'nvp-repl-filter)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-repl-filter.el ends here
