;;; nvp-parse.el --- Generics for parse -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-24 04:53:28>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created:  7 February 2019

;;; Commentary:
;; #<marker at 10467 in which-func.el.gz>
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'nvp-imenu)

;; -------------------------------------------------------------------
;;; utils

(eval-when-compile
  ;; imenu alists can be nested under headers:
  ;; ("Header" ("name" . marker)) or ("name" . marker)
 (defmacro nvp--parse-ensure-imenu ()
   "Create imenu index alist if possible."
   `(ignore-errors
      (when (and (fboundp 'imenu--make-index-alist) (null imenu--index-alist))
        (imenu--make-index-alist 'noerror)))))

;; -------------------------------------------------------------------
;;; Generics

;; default just tries to use imenu
(cl-defgeneric nvp-parse-function-names (&optional buffer-or-file &rest _args)
  "Default method to gather function names from current buffer or BUFFER-OR-FILE."
  ;; just loops through alist and gathers names
  (let ((buff (if (not buffer-or-file) (current-buffer)
                (if (buffer-live-p buffer-or-file)
                    buffer-or-file
                  (find-file-noselect buffer-or-file)))))
    (with-current-buffer buff
      (nvp--parse-ensure-imenu)
      (nvp-imenu-cleaned-alist))))

;; like which-func - attempt with imenu and add-log
(cl-defgeneric nvp-parse-current-function (&rest _args)
  "Default method to get name of function containing point.
First tries closest imenu entry, then `add-log-current-defun'."
  (nvp--parse-ensure-imenu)
  (let ((func (nvp-imenu-sort-relative-positions
               (point) (nvp-imenu-cleaned-alist))))
    (if func (caar func)
      (add-log-current-defun))))

(provide 'nvp-parse)
;;; nvp-parse.el ends here
