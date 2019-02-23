;;; nvp-doc.el --- doc strings/buffers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-22 19:27:29>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 10 February 2019

;;; Commentary:

;; Generic documentation lookup, modeled after xref
;; popup.el/pos-tip.el/quickhelp.el to truncate pop-tips
;; #<marker at 84732 in etags.el.gz>
;; #<marker at 8769 in cl-generic.el.gz>

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))

(cl-defstruct (nvp-doc (:constructor nvp-doc-make)
                       (:copier nil))
  "Documentation location information.
Location could be a buffer, file, or a function to call with no args.
START and END can be specify relevant region."
  buffer file func start end)

;;; TODO: default to getting capf / company documentation
(defvar nvp-doc-backend-functions nil
  "Special hook to find applicable doc backend for context.")

;;;###autoload
(defun nvp-doc-find-backend ()
  (run-hook-with-args-until-success 'nvp-doc-backend-functions))

(cl-defgeneric nvp-doc-backend-identifier-at-point (_backend)
  "Return identifier at point, a string or nil."
  (when-let ((ident (thing-at-point 'symbol)))
    (substring-no-properties ident)))

(cl-defgeneric nvp-doc-backend-identifier-completion-table (_backend)
  "Returns the completion table for identifiers."
  nil)

(cl-defgeneric nvp-doc-backend-doc (backend identifier)
  "Return doc for IDENTIFIER.")

(defvar nvp-doc--read-identifier-history nil)

(defun nvp-doc--read-identifier (prompt)
  "Return identifier at point or read from minibuffer."
  (let* ((backend (nvp-doc-find-backend))
         (id (nvp-doc-backend-identifier-at-point)))
    (cond ((or current-prefix-arg (not id))
           (completing-read
            (if id (format "%s (default %s): "
                           (substring prompt 0 (string-match "[ :]+\\'" prompt))
                           id)
              prompt)
            (nvp-doc-backend-identifier-completion-table backend)
            nil nil nil
            'nvp-doc--read-identifier-history id))
          (t id))))

;;;###autoload
(defun nvp-doc-at-point (identifier)
  (interactive (list (nvp-doc--read-identifier "Documentation for: ")))
  (nvp-doc--find-doc identifier 'popup))

(defun nvp-doc--show-doc (_doc _display-action)
  "Show DOC according to DISPLAY-ACTION.")

(defun nvp-doc--find-doc (input kind arg display-action)
  (let ((doc (funcall (intern (format "nvp-doc-backend-%s" kind))
                      (nvp-doc-find-backend)
                      arg)))
    (unless doc
      (user-error "No %s found for: %s" (symbol-name kind) input))
    (nvp-doc--show-doc doc display-action)))

(provide 'nvp-doc)
;;; nvp-doc.el ends here
