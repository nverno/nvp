;;; nvp-autoconf.el --- autoconf help at point -*- lexical-binding: t; -*-

;;; Commentary:

;; Help-at-point:
;; - Uses both URLs from company-autoconf and manual indicies as sources
;;   to gather info.
;; - Basic eldoc function for autoconf/m4

;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'company-autoconf)
(require 'nvp)
(nvp:decls)

;; -------------------------------------------------------------------
;;; Locally available macros

;; (defun nvp-autoconf-local-macros (&optional location)
;;   )


;; -------------------------------------------------------------------
;;; Eldoc

(defun nvp-autoconf-eldoc-function ()
  (when-let* ((sym (thing-at-point 'symbol))
              (sym (car-safe (member sym company-autoconf-keywords)))
              (annot (get-text-property 0 'annot sym)))
    (concat
     (propertize sym 'face 'font-lock-function-name-face) ": " annot)))

;; -------------------------------------------------------------------
;;; Manual lookup
;; PDF is converted to text and macros w/ indices are cached

;; cache manual indices here
(defvar nvp-autoconf-cache (expand-file-name "cache" user-emacs-directory))

;; location of manual
(defvar nvp-autoconf-manual (expand-file-name "autoconf.pdf" nvp-autoconf-cache))

;; regex to match macros and their page number in index
(defvar nvp-autoconf-macro-regexp "\\([A-Z0-9_]+\\)[ .]+\\([0-9]+\\)")

;;-- Manual utils

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
(nvp:define-cache-runonce nvp-autoconf-manual-index ()
  "Cached macro manual indicies."
  (nvp-autoconf--load-index))

(nvp:define-cache-runonce nvp-autoconf-all-macros ()
  "Cache of all known macros, either from company or index."
  (delete-dups (append (mapcar #'car (nvp-autoconf-manual-index))
                       company-autoconf-keywords)))

;; completing read for macros
;; by default completing read from all known macros
(defvar nvp-autoconf-read-history ())
(defun nvp-autoconf-read (&optional prompt default collection)
  (or default (setq default (thing-at-point 'symbol)))
  (completing-read
   (nvp:prompt-default (or prompt "Lookup macro: ") default)
   (if collection (funcall collection) (nvp-autoconf-all-macros))
   nil nil nil 'nvp-autoconf-read-history default))

;; -------------------------------------------------------------------
;;; Help commands 

;; Lookup MACRO in pdf manual
(defun nvp-autoconf-lookup-in-manual (&optional macro)
  "Lookup MACRO in manual."
  (interactive (list (nvp-autoconf-read nil nil 'nvp-autoconf-manual-index)))
  (require 'pdf-tools)
  (or macro (setq macro (nvp-autoconf-read nil nil 'nvp-autoconf-manual-index)))
  (when-let* ((page (cdr (assoc macro (nvp-autoconf-manual-index)))))
    (prog1 t
      (with-selected-window
          (display-buffer (find-file-noselect nvp-autoconf-manual 'nowarn))
        (with-no-warnings
          ;; +10 offset to first page
          (pdf-view-goto-page (+ 10 page)))))))

(defun nvp-autoconf-lookup (macro &optional online)
  "Lookup MACRO in the manual or ONLINE.
With prefix or if MACRO isn't in indices lookup ONLINE."
  (interactive (list (nvp-autoconf-read) current-prefix-arg))
  (or (and online (company-autoconf-location macro))
      (if (and (nvp-autoconf-lookup-in-manual macro) online)
          (message "No help found online for %s" macro)
        (message "No help found in manual or online for %s" macro))))

(defun nvp-autoconf-help-at-point (macro)
  "Display help popup for thing at point.
With prefix ARG, prompt with macros from manual."
  (interactive
   (list (let ((macro (thing-at-point 'symbol)))
           (if (or current-prefix-arg (not macro))
               (nvp-autoconf-read nil macro)
             macro))))
  (nvp:with-toggled-tip
    (if macro (or (get-text-property 0 'annot macro)
                  (format (if (assoc macro (nvp-autoconf-manual-index))
                              "Manual available for %s ('h')"
                            "No docs or index found for %s")
                          macro)))
    :help-fn (function (lambda (arg) (interactive "P") (nvp-autoconf-lookup macro arg)))
    :bindings (("." . (lambda () (interactive) (company-autoconf-location macro))))))

(provide 'nvp-autoconf)
;;; nvp-autoconf.el ends here
