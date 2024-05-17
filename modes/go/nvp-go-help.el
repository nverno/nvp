;;; nvp-go-help.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;; help-at-point for go source
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'go-mode nil t)
(nvp:decls :p (godoc godef) :f (godoc))

;; temporarily capture `package.function' as symbol
(defvar nvp-go-help-symbol-syntax
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?. "_" st)
    st))

;;; Godef
(defun nvp-go--godef-thingatpt ()
  (--when-let (godef--call (point))
    (let ((output (car it)))
      (when (and (godef--successful-p output)
                 (not (string-match-p "godef: .* not a valid identifier" output)))
        it))))

(defun nvp-hap-godef (command &optional arg &rest _args)
  (cl-case command
    (thingatpt (nvp-go--godef-thingatpt))
    (doc-string (mapconcat 'identity (butlast (cdr arg)) "\n"))))

;;; Godoc
(defun nvp-go--thingatpt ()
  (with-syntax-table nvp-go-help-symbol-syntax
    (bounds-of-thing-at-point 'symbol)))

;; call godoc synchronously for popups
(defun nvp-go-godoc (query command)
  (interactive (list (godoc--read-query) godoc-command))
  (let ((buf (with-current-buffer (get-buffer-create "*go-help*")
               (erase-buffer)
               (current-buffer))))
    (if (zerop (call-process-shell-command (concat command " " query) nil buf))
        buf
      (user-error "No help for %s" query))))

;; Hap backend for godef, go doc, and gogetdoc
;;;###autoload
(defun nvp-hap-godoc (command &optional arg &rest _args)
  (cl-case command
    (thingatpt (or (if (>= (prefix-numeric-value arg) 4)
                       (cons 'prompt (prefix-numeric-value arg)))
                   (nvp-go--thingatpt)))
    (doc-buffer
     (cl-letf (((symbol-function 'go--godoc) 'nvp-go-godoc))
       ;; call popup versions of godoc or gogetdoc
       ;; use point at end of symbol so fm|t.Printf returns help for
       ;; Printf
       (--when-let (pcase arg
                     (`(prompt . ,val)
                      (pcase val
                        (16 (call-interactively #'godoc))
                        (_
                         (let ((point
                                (or (ignore-errors (1- (cdr (nvp-go--thingatpt))))
                                    (point))))
                           (save-window-excursion
                             (let ((display-buffer-overriding-action
                                    '(nil . ((inhibit-switch-frame . t)))))
                               (window-buffer (godoc-gogetdoc point))))))))
                     (`(,_ . ,end) (godoc-and-godef (1- end))))
         (list it nil))))))

;;; Gogetdoc
(defun nvp-hap-gogetdoc (command &optional _arg &rest _args)
  (cl-case command
    (thingatpt (nvp-go--godef-thingatpt))
    (doc-buffer
     (save-window-excursion
       (list (window-buffer (godoc-gogetdoc (point))) nil)))))

(provide 'nvp-go-help)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-go-help.el ends here
