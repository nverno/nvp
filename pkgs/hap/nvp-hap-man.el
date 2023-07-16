;;; nvp-hap-man.el --- man help-at-point -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-hap)
(nvp:decls)
(nvp:auto "man" Man-completion-table)

(defvar nvp-hap-man--buffer "*hap-man*")

(defun nvp-hap-man-lookup (cmd &optional buffer async)
  "Get man output for CMD in BUFFER and return process. If ASYNC do asynchronously."
  (let ((cmd (concat "man --names-only " (regexp-quote cmd) " | col -b")))
    (if async
        (start-process-shell-command "man" (or buffer nvp-hap-man--buffer) cmd)
      (call-process-shell-command cmd nil (or buffer nvp-hap-man--buffer)))))

;;; TODO: getting symbol generic by mode
(defun nvp-hap-man-thingatpt ()
  (when-let ((sym (thing-at-point 'symbol)))
    (let* ((tmp (generate-new-buffer-name (concat " " nvp-hap-man--buffer)))
           (buff (get-buffer-create tmp)))
      (unwind-protect
          (with-current-buffer buff
            (progn
              (nvp-hap-man-lookup sym buff)
              (goto-char (point-min))
              (unless (looking-at-p "No manual")
                (when (buffer-live-p (get-buffer nvp-hap-man--buffer))
                  (kill-buffer nvp-hap-man--buffer))
                (rename-buffer nvp-hap-man--buffer)
                sym)))
        (and (equal tmp (buffer-name buff))
             (kill-buffer buff)
             (message "Killed %S" buff))))))


;;;###autoload
(defun nvp-hap-man (command &optional arg &rest _args)
  (cl-case command
    (thingatpt
     ;; only return thing at point with a prefix arg to explicity call man
     (when arg
       (nvp-hap-thing-at-point arg nil "Man: " 'Man-completion-table)))
    (doc-buffer)))


(provide 'nvp-hap-man)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-hap-man.el ends here
