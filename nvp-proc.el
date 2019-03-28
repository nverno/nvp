;;; nvp-proc.el --- process-related functions -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-27 20:56:52>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 24 March 2019

;;; Commentary:

;; - filters
;; - sentinels
;; - find processes

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'nvp)
(nvp-declare "" nvp-indicate-modeline)

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
