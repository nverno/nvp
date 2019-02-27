;;; nvp-autoconf.el --- autoconf help at point -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/make-tools
;; Last modified: <2019-02-26 00:48:34>
;; Package-Requires:
;; Created: 20 January 2017

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Uses both URLs from company-autoconf and manual indicies as sources
;; to gather info.
;; See x86-lookup.el for example of looking up indices in pdf manual.

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (nvp-local-vars)
  (require 'cl-lib)
  (require 'subr-x)
  (defvar company-autoconf-keywords))
(require 'nvp)
(require 'nvp-make)
(autoload 'company-autoconf-location "company-autoconf")

;; cache manual / index here
(defvar nvp-autoconf-cache (expand-file-name "cache" user-emacs-directory))

;; location of manual
(defvar nvp-autoconf-manual (expand-file-name "autoconf.pdf" nvp-autoconf-cache))

;; regex to match macros and their page number in index
(defvar nvp-autoconf-macro-regexp "\\([A-Z0-9_]+\\)[ .]+\\([0-9]+\\)")

;; -------------------------------------------------------------------
;;; Manual lookup

;; create index to lookup autoconf macros ((macro . page) ...)
(defun nvp-autoconf--create-index ()
  (when (file-exists-p nvp-autoconf-manual)
   (with-temp-buffer
     (call-process "pdftotext" nil t nil (file-truename nvp-autoconf-manual) "-")
     (goto-char (point-min))
     (search-forward "B.5 Autoconf Macro Index")
     (let (res)
       (while (not (or (looking-at-p "Appendix")
                       (eobp)))
         (and (looking-at nvp-autoconf-macro-regexp)
              (push (cons (concat "AC_" (match-string-no-properties 1))
                          (string-to-number (match-string-no-properties 2)))
                    res))
         (forward-line))
       (nvp-autoconf--save-index res)
       res))))

;; give cache a unique name
(defsubst nvp-autoconf--index-file ()
  (expand-file-name (sha1 nvp-autoconf-manual) nvp-autoconf-cache))

;; cache manual index -- makes cache directory if it doesn't exist
(defun nvp-autoconf--save-index (index)
  (unless (file-exists-p nvp-autoconf-cache)
    (make-directory nvp-autoconf-cache 'parents))
  (with-temp-file (nvp-autoconf--index-file)
    (prin1 index (current-buffer))))

;; load cached index
(defun nvp-autoconf--load-index ()
  (let ((index-file (nvp-autoconf--index-file)))
    (if (not (file-exists-p index-file))
        (nvp-autoconf--create-index)
      (with-temp-buffer
        (insert-file-contents index-file)
        (goto-char (point-min))
        (ignore-errors (read (current-buffer)))))))

;; load index, or if not made do all the setup and return it when finished
(nvp-define-cache-runonce nvp-autoconf-index ()
  "Cached macro manual indicies."
  (nvp-autoconf--load-index))

;; -------------------------------------------------------------------
;;; Commands

;; Lookup MACRO in pdf manual
;;;###autoload
(defun nvp-autoconf-lookup-manual (macro)
  (interactive (list (thing-at-point 'symbol)))
  (require 'pdf-tools)
  (unless macro
    (setq macro (nvp-completing-read "Lookup macro: " (nvp-autoconf-index))))
  (when-let* ((page (cdr (assoc macro (nvp-autoconf-index)))))
    (prog1 t
      (with-selected-window
          (display-buffer (find-file-noselect nvp-autoconf-manual 'nowarn))
        (with-no-warnings
          ;; +10 offset to first page
          (pdf-view-goto-page (+ 10 page)))))))

;; lookup VAR in manual or online
(defun nvp-autoconf-lookup (var &optional online)
  (interactive)
  (if online
      (company-autoconf-location var)
    (nvp-autoconf-lookup-manual var)))

;;;###autoload
(defun nvp-autoconf-help-at-point (arg)
  (interactive "P")
  (when-let* ((sym (thing-at-point 'symbol))
              (sym (car-safe (member sym company-autoconf-keywords))))
    (when arg
      (setq sym
            (nvp-completing-read "Lookup: " company-autoconf-keywords nil nil sym)))
    (nvp-with-toggled-tip
      (if sym (or (get-text-property 0 'annot sym) "No documentation found"))
      :help-fn (function (lambda (arg) (interactive "P") (nvp-autoconf-lookup sym arg)))
      :bindings (("." . (lambda () (interactive) (company-autoconf-location sym)))))))

;; -------------------------------------------------------------------
;;; Eldoc

;;;###autoload
(defun nvp-autoconf-eldoc-function ()
  (when-let* ((sym (thing-at-point 'symbol))
              (sym (car-safe (member sym company-autoconf-keywords)))
              (annot (get-text-property 0 'annot sym)))
    (concat
     (propertize sym 'face 'font-lock-function-name-face) ": " annot)))

(provide 'nvp-autoconf)
;;; nvp-autoconf.el ends here
