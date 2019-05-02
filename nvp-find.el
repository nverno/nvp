;;; nvp-find.el --- find stuff -*- lexical-binding: t; -*-

;;; Commentary:

;; Finding things:
;; - files
;; - occur

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro)
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
  (when-let* ((str (nvp-tap 'dwim)))
    (push (if (stringp str) (regexp-quote str) str) regexp-history))
  (if arg (nvp-multi-occur-in-this-mode)
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
