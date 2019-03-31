;;; nvp-find.el --- find stuff -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-03-31 09:34:28>
;; Created: 24 November 2016

;;; Commentary:

;; Finding things:
;; - files
;; - occur

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (require 'replace)
  (defvar recentf-list))
(require 'nvp-display)
(require 'nvp)
(autoload 'nvp-buffer-matching-mode "nvp-buffer")

;; -------------------------------------------------------------------
;;; Recentf 

;; Find a recent file using ido completion, only abbreviating
;; filenames.
;;;###autoload
(defun nvp-find-recentf (action)
  "Find recentf file, displaying with ACTION."
  (interactive "P")
  (if (not (bound-and-true-p recentf-mode))
      (recentf-mode))
  (let* ((file-assoc-list
	  (mapcar (lambda (x)
	            (cons (abbreviate-file-name x) x))
	          recentf-list))
	 (filename-list
	  (cl-remove-duplicates (mapcar #'car file-assoc-list) :test #'string=))
	 (filename (nvp-completing-read "Recent File: " filename-list nil t)))
    (when filename
      (nvp-display-location (cdr (assoc filename file-assoc-list))
                            :file (car action)))))

;; -------------------------------------------------------------------
;;; Occur 

;;;###autoload
(defun nvp-occur-dwim (&optional arg)
  "Call `occur' with either region or symbol-at-point.
With prefix ARG multi-occur in buffers of the same mode."
  (interactive "P")
  (if arg (nvp-multi-occur-in-this-mode)
    (when-let* ((str (nvp-region-str-or-thing 'symbol)))
      (push (if (stringp str) (regexp-quote str) str) regexp-history))
    (call-interactively 'occur)
    (if (get-buffer "*Occur*")
        (switch-to-buffer-other-window "*Occur*"))))

;; https://www.masteringemacs.org/article/searching-buffers-occur-mode
;; Show all lines matching REGEXP in buffers withe the same 
;; `major-mode'.
;;;###autoload
(defun nvp-multi-occur-in-this-mode ()
  (interactive)
  (multi-occur
   (nvp-buffer-matching-mode major-mode)
   (car (occur-read-primary-args))))

(provide 'nvp-find)
;;; nvp-find.el ends here
