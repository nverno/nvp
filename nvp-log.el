;;; nvp-log.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; FIXME: get rid of this, find better strategy
;; basic logging mode
;; - ert message capturing: #<marker at 10762 in ert-x.el.gz>
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)

(defvar nvp-log-buffer "*nvp-log*")

;;;###autoload
(defun nvp-log (&rest args)
  (apply nvp-default-log-function args))

;; initialize new log buffer and return it
(defun nvp-log-get-buffer (&optional buffer-name)
  (or buffer-name (setq buffer-name nvp-log-buffer))
  (or (get-buffer buffer-name)
      (let ((buff (get-buffer-create buffer-name)))
       (prog1 buff
         (set-buffer buff)
         (nvp-log-mode)))))

(defmacro nvp-with-log-buffer (&optional buffname &rest body)
  `(with-current-buffer (nvp-log-get-buffer ,buffname)
     (goto-char (point-max))
     (let ((inhibit-read-only t))
       ,@body)))

;;;###autoload
(defun nvp-log-default (text &optional buffer-name &rest args)
  (let (deactivate-mark)
    (nvp-with-log-buffer buffer-name
     (insert
      (apply #'format (replace-regexp-in-string "[\r\n]+" "\n" text) args)))))

;; ------------------------------------------------------------
;;; Mode

(require 'compile)

(defvar nvp-log--font-lock-keywords
  `(("`\\([^\n']+\\)'" (1 font-lock-constant-face))
    (,(rx symbol-start (or "finished" "deleted" "open" "success") symbol-end)
     . compilation-info-face)
    (,(rx symbol-start "warning" symbol-end) . compilation-warning-face)
    (,(rx symbol-start (or "exited abnormally" "failed" "error"
                  "signal-description" "connection broken")
          symbol-end)
     . compilation-line-number)
    (,(rx symbol-start (or "run" "open" "listen" "connect") symbol-end)
     . compilation-info-face)
    (,(rx symbol-start (or "stop" "exit" "signal" "closed" "failed") symbol-end)
     . compilation-line-number)))

;;;###autoload
(define-derived-mode nvp-log-mode special-mode "Log"
  "Logging mode."
  :abbrev-table nil
  (setq-local read-only-mode nil)
  (setq-local font-lock-defaults (list nvp-log--font-lock-keywords nil t)))

(provide 'nvp-log)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-log.el ends here
