;;; nvp-tag.el --- tagging utilities -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-27 01:46:53>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created:  7 February 2019

;;; Commentary:

;; TODO:
;; - etags
;; - ctags
;; - gtags
;; - tags from imenu?
;; - tag directory
;; - tag dwim
;;
;; #<marker at 84484 in etags.el.gz>

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'nvp-macro)
  (require 'tramp))
(require 'nvp)
(defvar imenu-generic-expression)

;; find-tag default functions: #<marker at 114864 in subr.el.gz>
(defun nvp-tag-get-default ()
  (or (and (region-active-p)
           (/= (point) (mark))
           (buffer-substring-no-properties (point) (mark)))
      (funcall (or find-tag-default-function
                   (get major-mode 'find-tag-default-function)
                   'find-tag))))

(cl-defgeneric nvp-tag-command ()
  "Command to generate tags.")

;; -------------------------------------------------------------------
;;; Using imenu
;; ess #<marker at 24008 in ess-utils.el>
;; grep #<marker at 31024 in grep.el.gz>

(defvar nvp-tag-find-template nil
  "Compute find command, ala `grep-find-template'.
Placeholders:
 <D> - base directory for find
 <X> - find options to restrict or expand directory list
 <F> - find options to limit files matched
 <R> - regular expression to search for")

(defvar nvp-tag-find-command nil)

;; ;;;###autoload
;; (defun nvp-tag-directory-imenu (dir tagfile)
;;   "Use imenu regexp to call find .. | etags .. in shell command."
;;   (interactive "DDirectory to tag:
;; GTags file (default TAGS): ")
;;   (when (or (eq (length (file-name-nondirectory tagfile) 0))
;;             (file-directory-p tagfile))
;;     (setq tagfile (concat (file-name-as-directory tagfile) "TAGS")))
;;   (when (file-remote-p tagfile)
;;     (require 'tramp)
;;     (setq tagfile (with-parsed-tramp-file-name tagfile foo foo-localname)))
;;   (when (file-remote-p dir)
;;     (require 'tramp)
;;     (setq dir (with-parsed-tramp-file-name dir foo foo-localname)))
;;   (unless imenu-generic-expression
;;     (error "No `imenu-generic-expression' defined for %s" major-mode))
;;   (let* ((find-cmd
;;           (format "find %s -type f -size 1M \\( -regex \".*\\.\"")))))

(provide 'nvp-tag)
;;; nvp-tag.el ends here
