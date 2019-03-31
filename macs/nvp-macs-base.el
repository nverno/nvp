;;; nvp-macs-base.el --- basic macros -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-30 22:49:21>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 30 March 2019

;;; Commentary:
;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'macroexp)

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
;;; Define 

(defmacro nvp-defvar (&rest var-vals)
  "Define VAR and eval VALUE during compile."
  (declare (indent 0))
  (macroexp-progn
   (cl-loop for (var value) on var-vals by #'cddr
      collect `(progn (defvar ,var (eval-when-compile ,value))))))

(defmacro nvp-setq (&rest var-vals)
  "Define VAR and eval VALUE during compile."
  (declare (indent 0))
  (macroexp-progn
   (cl-loop for (var value) on var-vals by #'cddr
      collect `(progn (eval-when-compile (defvar ,var))
                 (setq ,var (eval-when-compile ,value))))))

(provide 'nvp-macs-base)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-macs-base.el ends here
