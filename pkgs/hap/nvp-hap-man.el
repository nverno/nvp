;;; nvp-hap-man.el --- man help-at-point -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-hap)
(require 'man)
(nvp:decls)

(defvar nvp-hap-man--buffer "*hap-man*")

(defun nvp-hap-man-lookup (cmd &optional buffer async)
  "Get man output for CMD in BUFFER and return process. If ASYNC do asynchronously."
  (let ((cmd (concat "man --names-only " (regexp-quote cmd) " | col -b")))
    (if async
        (start-process-shell-command "man" (or buffer nvp-hap-man--buffer) cmd)
      (call-process-shell-command cmd nil (or buffer nvp-hap-man--buffer)))))

(cl-defgeneric nvp-hap-man-thing-at-point ()
  (thing-at-point 'symbol))

(nvp:defmethod nvp-hap-man-thing-at-point ()
  :modes (c++-mode c++-ts-mode)
  "Append std:: to lookup c++ man docs (stdman stdlib man pages)."
  (when-let ((sym (thing-at-point 'symbol)))
    (unless (string-prefix-p "std::" sym)
      (concat "std::" sym))))

;; only return thing at point with a prefix arg to explicity call man or if
;; there is a unique match for "thing", possibly followed by "(num)" for section
(defun nvp-hap-man-thingatpt (&optional arg)
  (if arg
      (nvp-hap-thing-at-point arg nil "Man: " 'Man-completion-table)
    (let* ((sym (nvp-hap-man-thing-at-point))
           (comps (Man-completion-table sym nil nil)))
      (when (or (eq t comps)
                (and (stringp comps)
                     (string-match-p (concat sym "\\(?:(\\d*)?\\)?") sym)))
        sym))))

;;;###autoload
(defun nvp-hap-man-buffer (topic)
  (let* ((Man-notify-method 'quiet)
         ;; added in emacs v30!
         (Man-prefer-synchronous-call t)
         (buf (man topic)))
    (and (buffer-live-p buf)
         (list buf (point-min) nil))))

;;;###autoload
(defun nvp-hap-man (command &optional arg &rest _args)
  (cl-case command
    (thingatpt (nvp-hap-man-thingatpt arg))
    (doc-buffer (nvp-hap-man-buffer arg))))

(provide 'nvp-hap-man)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-hap-man.el ends here
