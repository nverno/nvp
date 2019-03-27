;;; nvp-font.el --- add/toggle additional font-locking -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-27 10:10:23>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 29 November 2016

;;; Commentary:

;; Utils to add additional font-locking

;; TODO:
;; - update a mode's additions w/o creating entire new entry
;; - how to remove font locks added with mode via `font-lock-add-keywords'?

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'nvp)

;; -------------------------------------------------------------------
;;; Fontify things in quoted regions

;; Non-nil if point is inside a CHAR quoted region
(defsubst nvp-font-lock-quoted-p (&optional char)
  (eq (nth 3 (syntax-ppss)) (or char ?\")))

;; add fontification to REGEX up to LIMIT in quoted area by CHAR
;; (default double-quotes)
(defsubst nvp-fontify-quoted-1 (regex char limit)
  (let (res)
    (while (and (setq res (re-search-forward regex limit 'move))
                (not (nvp-font-lock-quoted-p char))))
    res))

(cl-defmacro nvp-fontify-quoted (&rest forms &key char &allow-other-keys)
  "Fontify elements in quoted regions."
  (declare (indent defun))
  (while (keywordp (car forms))
    (setq forms (cdr (cdr forms))))
  (unless char (setq char ?\"))
  (macroexp-progn
   (cl-loop for (regex font) in forms
      collect `(cons ,(apply-partially #'nvp-fontify-quoted-1 regex char)
                     '(,font)))))

;; -------------------------------------------------------------------
;;; Create font-lock additions

(defmacro nvp-font-lock-keywords (&rest forms)
  "Create list of font-lock additions.
Each element of FORMS is a list ([:quoted char] regex font-spec)."
  (declare (indent defun))
  `(list
    ,@(cl-loop for form in forms
         as quoted = (plist-get form :quoted)
         do (while (keywordp (car form))
              (setq form (cdr (cdr form))))
         if quoted
         collect `(nvp-fontify-quoted :char ,quoted ,form)
         else
         collect `(cons ,(car form) ',(cdr form)))))

(defmacro nvp-font-lock-add-defaults (mode &rest forms)
  "Add font-lock additions to MODE."
  (declare (indent defun))
  (macroexp-let2 nil fonts `(progn (nvp-font-lock-keywords ,@forms))
   `(progn
      (cl-pushnew (cons ,mode ,fonts) nvp-mode-font-additions :test #'equal)
      (font-lock-add-keywords ,mode ,fonts))))

;; -------------------------------------------------------------------
;;; Glyphs 

(defun nvp-font-quote-glyphs ()
  (let ((tbl (make-display-table)))
    (aset tbl 8220 (vector (make-glyph-code ?\" 'default)))
    (aset tbl 8221 (vector (make-glyph-code ?\" 'default)))
    (aset tbl 8216 (vector (make-glyph-code ?\` 'default)))
    (aset tbl 8217 (vector (make-glyph-code ?\' 'default)))
    (setq standard-display-table tbl)))

(defun nvp-font-glyphify (item glyph)
  `((,item
     (0 font-lock-keyword-face t)
     (0 (prog1
            (compose-region (match-beginning 0)
                            (match-end 0)
                            ,glyph)
          nil)))))

(provide 'nvp-font)
;;; nvp-font.el ends here
