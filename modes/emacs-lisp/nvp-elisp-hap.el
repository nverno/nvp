;;; nvp-elisp-hap.el --- popup help at point -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/elisp-utils
;; Last modified: <2019-03-15 09:24:15>
;; Created: 31 October 2016

;;; Commentary:

;; TODO:
;; - merge with nvp-with-toggled-tip, most of this is redundant
;; - generic method to get documentation / help-at-point

;;; Code:
(require 'pos-tip)

;; Number of lines to show in popup.
(defvar nvp-elisp-hap-max-lines 20)

;; non-nil if popups should use gtk tooltips
(defvar nvp-elisp-hap-use-gtk nil)

;; default display time
(defvar nvp-elisp-hap-timer 20)

;; ------------------------------------------------------------
;;; General

;; Get the doc for thing at point applying ACTION
(defun nvp-elisp-hap--help (action)
  (let ((sym (symbol-at-point)))
    (unless sym
      (user-error "No thing is found here."))
    (condition-case nil
        (funcall action sym)
      (t (user-error "No help found for %s" (symbol-name sym))))))

;; Show documentation for thing at point in popup window.
(defun nvp-elisp-hap--doc (result)
  (unless result
    (user-error "Nothing to see here."))
  (pos-tip-show result nil nil nil nvp-elisp-hap-timer))

;; make temporary key binding
(defun nvp-elisp-hap--temp-binding (key command &optional keep exit)
  (set-transient-map
   (let ((tmap (make-sparse-keymap)))
     (define-key tmap (kbd key) command)
     tmap)
   (or keep t)
   (or exit nil)))

;; ------------------------------------------------------------
;;; elisp specific

;; Get documentation for SYMBOL
(defun nvp-elisp-hap--elisp-doc (symbol)
  (let* ((doc (if (fboundp symbol)
                  (documentation symbol)
                (documentation-property symbol 'variable-documentation))))
    (if (stringp doc)
        (with-temp-buffer
          (insert doc)
          (let* ((lines (count-lines (point-min) (point-max)))
                 (end (min lines nvp-elisp-hap-max-lines)))
            (buffer-substring-no-properties
             (point-min)
             (point-at-bol end))))
      (user-error "No help found for '%s'" (symbol-name symbol)))))

(defun nvp-elisp-hap--elisp-buff ()
  "Give help buffer for symbol at point."
  (interactive)
  (let ((x-gtk-use-system-tooltips nvp-elisp-hap-use-gtk)
        (symbol (symbol-at-point)))
    (x-hide-tip)
    (cond ((fboundp symbol) (describe-function symbol))
          ((boundp symbol) (describe-variable symbol))
          (t (user-error "Thing unknown...")))))

;;;###autoload
(defun nvp-elisp-help-at-point ()
  "Toggle popup help documentation for thing at point using `pos-tip'."
  (interactive)
  (let ((x-gtk-use-system-tooltips nvp-elisp-hap-use-gtk))
    (or (x-hide-tip)
        (let ((res (nvp-elisp-hap--help #'nvp-elisp-hap--elisp-doc)))
          (when res
            (nvp-elisp-hap--doc res)
            (nvp-elisp-hap--temp-binding "h" #'nvp-elisp-hap--elisp-buff)
            (nvp-elisp-hap--temp-binding "q" #'(lambda () (interactive) (x-hide-tip))))))))

(provide 'nvp-elisp-hap)
;;; nvp-elisp-hap.el ends here
