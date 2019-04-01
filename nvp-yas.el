;;; nvp-yas.el --- snippet helpers -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-04-01.07>
;; Created: 20 December 2016

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (nvp-local-vars))
(require 'nvp)
(require 'yasnippet)
(require 'nvp-comment)
(autoload 'nvp-parse-current-function "nvp-parse")

(defalias 'yas-comment-string 'nvp-yas-comment)

;; -------------------------------------------------------------------
;;; Buffers / files

;; filename w/o directory
(defsubst nvp-yas-bfn ()
  (nvp-path 'bfns :or-name t))

;; filename w/o dir. or ext.
(defsubst nvp-yas-bfn-no-ext ()
  (nvp-path 'bfnse :or-name t))

;; containing directory name only
(defsubst nvp-yas-dfn ()
  (nvp-path 'dn))

;; current indentation
(defsubst nvp-yas-indent ()
  (current-indentation))

;; -------------------------------------------------------------------
;;; Padding / Headers

;; create string length of `yas-text', optionally constrained by min-len/max-len
(defsubst nvp-yas-header (char &optional min-len max-len)
  (let ((sw (+ (or min-len 0) (string-width yas-text))))
    (make-string (if max-len (max 0 (min sw (- max-len sw))) sw) char)))

;; add padding to `yas-text'
(defsubst nvp-yas-pad (char padmin padmax)
  (let* ((sw (+ padmin (string-width yas-text)))
         (extra (max 0 sw (- padmax sw))))
    (make-string (/ extra 2) char)))

;; fill after yas-text with CHAR until PADMAX
(defsubst nvp-yas-pad-right (char padmax)
  (make-string (max 0 (- padmax (string-width yas-text))) char))

;; center `yas-text' constrained by padmin/padmax
(defsubst nvp-yas-center (padmin padmax &optional char)
  (or char (setq char ? ))
  (let* ((sw (+ padmin (length yas-text)))
         (extra (max 0 sw (- padmax sw))))
    (concat (make-string (ceiling extra 2) char)
            yas-text
            (make-string (floor extra 2) char))))

;; -------------------------------------------------------------------
;;; General

;; yas-inside-string uses `font-lock-string-face'
(defsubst nvp-yas-in-string ()
  (nth 3 (syntax-ppss)))

(defsubst nvp-yas-in-comment ()
  (nth 4 (syntax-ppss)))

(defsubst nvp-yas-in-string-or-comment ()
  (nvp-ppss 'soc))

;; -------------------------------------------------------------------
;;; Functions, args, variables

;; Function arguments w/o types
;; split argument STR by SEPS, eg. "a,b" => '("a" "b"). Strings are trimmed and
;; nulls are dropped.
;; if DEFAULTS is non-nil, split by "=" as well, eg.
;; "a,b=1," => '("a" ("b" "1"))
(defun nvp-yas-split-args (str &optional seps defaults)
  (let ((args (split-string str (or seps "[ \t]*,[ \t]*") t " ")))
    (if defaults
        (mapcar (lambda (s)
                  (let ((defs (split-string s "[ \t]*=[ \t]*" t " ")))
                    (if (= (length defs) 1) (car defs) defs)))
                args)
      args)))

;; Variable that may/may not have a type, eg in a for loop
;; Gets variable name from string of form "i = 1" or "int i = 1"
(defun nvp-yas-var (str)
  (if (< (length str) 1)
      ""
   (let* ((str (car (split-string str "=" t " ")))
          (strs (split-string str nil t " ")))
     (or (cadr strs) (car strs)))))

;; name of current function or script
(defun nvp-yas-function-or-script ()
  (or (nvp-parse-current-function) (nvp-path 'bfnse :or-name t)))

;; build param strings: params range from BEG length LEN
;; each param is prepended by JOIN string
(defsubst nvp-yas-param-str (beg len join &optional fmt)
  (or fmt (setq fmt "$%d"))
  (if (or (not len) (= len 0)) ""
    (concat join
            (mapconcat (lambda (n) (format fmt n))
                       (number-sequence beg (+ beg (1- len))) join))))

;; -------------------------------------------------------------------
;;; Or patterns

(defsubst nvp-yas-or-values (str &optional seps)
  (split-string str (or seps "[\]\[|]") 'omit " "))

;; -------------------------------------------------------------------
;;; Input

;; yas read input wrapper
(defun nvp-yas-read (func &rest args)
  (unless (or yas-moving-away-p
              yas-modified-p)
    (apply func args)))

(provide 'nvp-yas)
;;; nvp-yas.el ends here
