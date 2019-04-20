;;; nvp-proc.el --- process-related functions -*- lexical-binding: t; -*-

;;; Commentary:

;; - filters
;; - sentinels
;; - find processes

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'nvp)
(nvp-decls)

(defmacro nvp-with-proc (proc &rest body)
  "Execute BODY with current buffer PROC if process is live."
  (declare (indent defun) (debug t))
  `(if-let ((,proc (nvp-buffer-process)))
       (if (not (process-live-p ,proc))
           (user-error "Buffer process is not live.")
         ,@body)
     (user-error "Current buffer has no process.")))

;; -------------------------------------------------------------------
;;; Find processes

;; find first item using TEST function (default 'equal)
(cl-defsubst nvp-proc-find (item &key test key)
  (let ((completion-ignore-case t)
        (case-fold-search t))
    (cl-find item (process-list) :test (or test #'equal) :key key)))

;; find first process matched by PRED function
(cl-defsubst nvp-proc-find-if (pred &key key start end from-end)
  (and pred (cl-find-if pred (process-list) :key key :start start :end end
                        :from-end from-end)))

;; find all processes matching PRED
(defun nvp-proc-find-all (pred)
  (cl-loop for proc in (process-list)
     when (funcall pred proc)
     collect proc))

;; find process by matching NAME
(defsubst nvp-proc-find-by-name (name)
  (nvp-proc-find name :test #'string-match-p :key #'process-name))

;; -------------------------------------------------------------------
;;; Filters / Sentinels

;; condense multiple newlines
;;;###autoload
(defun nvp-proc-default-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert (replace-regexp-in-string "[\r\n]+" "\n" string))))

;; log proc & exit status. Kill buffer or RETURN it on success.
;; On failure, jump to process-buffer
;;;###autoload
(defun nvp-proc-default-sentinel (&optional return)
  (lambda (p m)
    (let ((pname (process-name p)))
      (funcall nvp-default-log-function "%s: %s" nil pname m)
      (with-current-buffer (process-buffer p)
        (if (not (zerop (process-exit-status p)))
            (progn (nvp-indicate-modeline pname 'failure)
                   (pop-to-buffer (current-buffer)))
          (nvp-indicate-modeline pname)
          (if return (current-buffer)
            (kill-buffer (current-buffer))))))))

;; generate and return a new comint buffer
(defsubst nvp-proc-comint-buffer (name)
  (let ((buff (generate-new-buffer name)))
    (set-buffer buff)
    (comint-mode)
    buff))

(provide 'nvp-proc)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-proc.el ends here
