;;; nvp-macs-common.el --- basic macros -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-31 05:53:12>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 30 March 2019

;;; Commentary:
;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'macroexp)

;; -------------------------------------------------------------------
;;; Helpers

(defun nvp--normalize-modemap (mode &optional minor)
  "Convert MODE to keymap symbol if necessary.
If MINOR is non-nil, create minor mode map symbol."
  (if (keymapp mode) mode
    (and (symbolp mode) (setq mode (symbol-name mode)))
    (let ((minor (or minor (string-match-p "-minor" mode))))
      (if (not (or (string-match-p "-map\\'" mode)
                   (string-match-p "-keymap\\'" mode)))
          (intern
           (concat (replace-regexp-in-string "\\(?:-minor\\)?-mode\\'" "" mode)
                   (if minor "-minor-mode-map" "-mode-map")))
        (intern mode)))))

(defun nvp--normalize-hook (mode &optional minor)
  "Convert MODE to canonical hook symbol.
If MINOR is non-nil, convert to minor mode hook symbol."
  (and (symbolp mode) (setq mode (symbol-name mode)))
  (let ((minor (or minor (string-match-p "-minor" mode))))
    (intern
     (concat
      (replace-regexp-in-string
       "\\(?:-minor-\\)?\\(?:-mode\\)?\\(?:-hook\\)?\\'" "" mode)
      (if minor "-minor-mode-hook" "-mode-hook")))))

;; -------------------------------------------------------------------
;;; OS 

(defmacro nvp-with-w32 (&rest body)
  (declare (indent 0) (debug t))
  (when (eq system-type 'windows-nt)
    `(progn ,@body)))

(defmacro nvp-with-gnu (&rest body)
  (declare (indent 0) (debug t))
  (when (not (eq system-type 'windows-nt))
    `(progn ,@body)))

(defmacro nvp-with-gnu/w32 (gnu w32)
  (declare (indent 2) (indent 1) (debug t))
  (if (eq system-type 'windows-nt)
      `,@w32
    `,@gnu))

;; -------------------------------------------------------------------
;;; Compat 

(unless (fboundp 'ignore-errors)
  (defmacro ignore-errors (&rest body)
    `(condition-case nil (progn ,@body) (error nil))))

(defvar eieio--known-slot-names)
(defmacro eieio-declare-slot (name)
  (cl-pushnew name eieio--known-slot-names) nil)

;; -------------------------------------------------------------------
;;; Declares / Autoloads
;; silence byte-compiler warnings

(defalias 'nvp-decl 'nvp-declare)
(defmacro nvp-declare (package &rest funcs)
  (declare (indent defun))
  (macroexp-progn
   (cl-loop for func in funcs
      collect `(declare-function ,func ,package))))

(defmacro nvp-autoload (package &rest funcs)
  (declare (indent defun))
  (macroexp-progn
   (cl-loop for func in funcs
      collect `(autoload ',func ,package))))

;; -------------------------------------------------------------------
;;; Conversion

;;; FIXME: remove -- seem to just occur in C stuff
(defmacro nvp-listify (args)
  "Ensure ARGS is a list."
  (let ((args (if (stringp args) (intern args) args)))
    `(unless (consp ,args) (setq ,args (cons ,args nil)))))

(defmacro nvp-string-or-symbol (sym)
  "If SYM is string convert to symbol."
  `(if (stringp ,sym) (intern ,sym) ,sym))

(defmacro nvp-stringify (name)
  "Sort of ensure that NAME symbol is a string."
  `(progn
     (pcase ,name
       ((pred stringp) ,name)
       ((pred symbolp) (symbol-name ,name))
       (`(quote ,sym) (symbol-name sym))
       (`(function ,sym) (symbol-name sym))
       (_ (user-error "How to stringify %S?" ,name)))))

;; -------------------------------------------------------------------
;;; Files / buffers

(defmacro nvp-file-same (file-1 file-2)
  "Return non-nil if FILE-1 and FILE-2 are the same."
  (declare (indent defun))
  `(when (and (file-exists-p ,file-1) (file-exists-p ,file-2))
     (equal (file-truename ,file-1) (file-truename ,file-2))))

;; modified from smartparens.el
;; (defmacro nvp-with-buffers-using-mode (mode &rest body)
;;   "Execute BODY in every existing buffer using `major-mode' MODE."
;;   (declare (indent 1))
;;   `(dolist (buff (buffer-list))
;;      (when (provided-mode-derived-p ,mode (buffer-local-value 'major-mode buff))
;;        (with-current-buffer buff
;;          ,@body))))

;;; Load file name
(defmacro nvp-load-file-name ()
  "Expand to the file's name."
  '(cond
    (load-in-progress load-file-name)
    ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
     byte-compile-current-file)
    (t (buffer-file-name))))

(provide 'nvp-macs-common)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-macs-common.el ends here
