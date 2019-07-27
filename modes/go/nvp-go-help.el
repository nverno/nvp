;;; nvp-go-help.el ---  -*- lexical-binding: t; -*-

;;; Commentary:

;; help-at-point for go source

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (defvar godoc-command))
(require 'nvp-go)
(declare-function godoc-and-godef "go-mode")
(declare-function godoc-at-point "go-mode")
(declare-function godoc-gogetdoc "go-mode")
(declare-function godoc--read-query "go-mode")
(autoload 'pos-tip-show "pos-tip")

;; if non-nil use gtk tooltips
(defvar nvp-go-use-gtk nil)

;; -------------------------------------------------------------------
;;; Utils

(eval-when-compile
  ;; toggle help popup with pos-tip
  (cl-defmacro with-toggled-tip (&key help-doc type highlight)
    (declare (indent defun) (debug t))
    `(let ((x-gtk-use-system-tooltips nvp-go-use-gtk))
       (or (x-hide-tip)
           (let ((str ,help-doc))
             ;; FIXME: pos-tip can't display font-lock? or losing
             ;;        properties somehow
             ;; if HIGHLIGHT: when TYPE is non-nil and matches STR,
             ;; then propertize the match in STR
             ,(when highlight
                `(when ,type (string-match ,type str)
                       (add-text-properties
                        (match-beginning 0) (match-end 0)
                        (list 'face 'font-lock-warning-face) str)))
             (pos-tip-show str nil nil nil 10))))))

;; -------------------------------------------------------------------
;;; Help toggles

;; string formatting
(defun nvp-go-help-string-format ()
  (interactive)
  (with-toggled-tip
    :help-doc "Formatting codes:

%d          | decimal integer
%x, %o, %b  | integer in hex, octal, binary
%f, %g, %e  | float
%t          | bool
%c          | rune (Unicode code point)
%s          | string
%q          | quoted string \"abc\" or rune 'c'
%v          | any value in a natural format
%T          | type of any value
%%          | literal percent sign

Printf notes:
%[digit]	tells Printf to use digit'th operand.
#		for %o, %x, %X emit a 0, 0x, or 0X prefix"))

(defun nvp-go-help-types (&optional type)
  (interactive)
  (with-toggled-tip
    :highlight t
    :type type
    :help-doc "Types:

int[8,16,32,64]		| int(2's complement)/uint determined by -
uint[8,16,32,64]	| - compiler to be most efficient for arch
rune			| synonym for int32 (Unicode code point)
byte			| synonym for uint8 (raw data)
uintptr			| width unspec. - holds bits of pointer value
float[32,64]		| IEEE 754 standard, precision[~6,~15]

Notes:
Signed integers are 2's complement.
Special floats: NaN, +Inf, -Inf. Use math.IsNaN, math.NaN"))

(defun nvp-go-help-ops ()
  (interactive)
  (with-toggled-tip
    :help-doc "Ops:

%		| Only applies to integers. Sign of remainder is -
		| - always same as dividend: -5%3 and -5%-3 => -2.
/		| Integer division when applied to ints.
&^		| bit clear (AND NOT)"))

;; -------------------------------------------------------------------
;;; Help at point

;;; godoc functions
;; redefine so they dont pop to buffers
;; hijack `godoc-gogetdoc'
(defun nvp-go-help-gogetdoc-at-point (point)
  (interactive "d")
  (cl-letf (((symbol-function 'display-buffer)
             #'(lambda (&rest _ignored)
                 (replace-regexp-in-string "[\r\n]+" "" (buffer-string)))))
    (with-toggled-tip :help-doc (godoc-gogetdoc point))))

;; call godoc synchronously for popups
(defun nvp-go-help--godoc (query command)
  (interactive (list (godoc--read-query) godoc-command))
  (call-process-shell-command
   (concat command " " query) nil "*go-help*")
  (with-current-buffer "*go-help*"
    (prog1 (replace-regexp-in-string "[\n\r]+$" "" (buffer-string))
      (kill-buffer))))

(defun nvp-go-help-godoc-at-point (point)
  (interactive "d")
  (cl-letf (((symbol-function 'go--godoc) 'nvp-go-help--godoc))
    (with-toggled-tip :help-doc (godoc-and-godef point))))

;; temporarily capture `package.function' as symbol
(defvar nvp-go-help-symbol-syntax
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?. "_" st)
    st))

;;;###autoload
(defun nvp-go-help-at-point (arg)
  (interactive "P")
  (with-syntax-table nvp-go-help-symbol-syntax
    (let* ((bnds (bounds-of-thing-at-point 'symbol))
           (sym (thing-at-point 'symbol)))
      (cond
       ((null sym)
        (call-interactively 'godoc))
       ;; Formatted strings
       ((nth 3 (syntax-ppss))
        (save-excursion
          ;; back out of string and function call
          (ignore-errors (up-list -2 t))
          ;; if we in string in formatting function
          (and (string-match-p
                "\\(?:fmt\\|log\\).*f$" (thing-at-point 'symbol))
               (nvp-go-help-string-format))))
       ;; Types
       ((string-match-p go-tools-type-re sym)
        (nvp-go-help-types))
       ;; call popup versions of godoc or gogetdoc
       ;; use point at end of symbol so fm|t.Printf returns help for
       ;; Printf
       (t
        (if arg
            (funcall-interactively #'nvp-go-help-gogetdoc-at-point (1- (cdr bnds)))
          (funcall-interactively #'nvp-go-help-godoc-at-point (1- (cdr bnds)))))))))

(provide 'go-help)
;;; nvp-go-help.el ends here
