;;; nvp-makefile.el --- make helpers -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/make-tools
;; Last modified: <2019-03-27 11:18:17>
;; Created:  3 November 2016

;;; Commentary:

;; TODO:
;; - font-lock-doc-face for info/warning/error?
;; - incorporate semantic stuff?
;; - use info-completition-at-point function
;;   #<marker at 25312 in info-look.el.gz>
;; - align rules => similar to sh-mode rules

;; FIXME:
;; - beginning/end of defun functions don't work
;; - collect remote info async

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'subr-x)
  (require 'cl-lib))
(require 'nvp)
(require 'make-mode)

;; -------------------------------------------------------------------
;;; Util

(eval-when-compile
  (defmacro nvp-makefile-with-target (target &rest body)
    "Execute BODY with point after ':' following TARGET."
    (declare (indent defun) (debug (symbolp &rest form)))
    `(save-excursion
       ;; if target is found point will be at the end
       ;; of match, skip ahead to ':'
       (when (nvp-makefile-goto-target ,target)
         (skip-chars-forward "^:" (point-at-eol))
         (forward-char 1)
         ,@body))))

;; dont tab out when inserting comments
(define-advice comment-dwim (:around (orig-fn &rest args) "space-to-comment")
  (let ((indent-tabs-mode nil))
    (apply orig-fn args)))

;; -------------------------------------------------------------------
;;; Font-lock
;; TODO:
;; - info/warn/error
;; - remove string fontification in #define blocks where it is incorrect.

;; ------------------------------------------------------------
;;; Web topics

;; FIXME: run async
;; collect matches from url
(defun nvp-makefile-collect-topics (url regex)
  (let (res)
    (nvp-while-scanning-url url regex
      (push (match-string-no-properties 1) res))
    res))

(nvp-define-cache-runonce nvp-makefile-special-targets ()
  "List of special make targets."
 ;; propertize :manual (concat url (match-string 1))
 (nvp-makefile-collect-topics
  "https://www.gnu.org/software/make/manual/html_node/Special-Targets.html"
  "dt[>< ]+code[<> ]+\\([.A-Za-z]+\\)"))

;; -------------------------------------------------------------------
;;; General mode variables

;; FIXME: these don't work correctly at all
(defun nvp-makefile-beginning-of-defun-function (&optional arg)
  "See `beginning-of-defun'.
Treats target blocks as defuns."
  (when (or (null arg) (= 0 arg)) (setq arg 1))
  (while (and (not (= 0 arg))
              (makefile-previous-dependency))
    (setq arg (if (> arg 0) (1- arg) (1+ arg)))))

(defun nvp-makefile-end-of-defun-function ()
  "See `end-of-defun'.
Skips to end of tabbed block."
  (forward-line 1)                   ;called when point is at beginning of block
  (while (looking-at-p "^\t")
    (forward-line 1)))

;; ------------------------------------------------------------
;;; Parse / Snippet helpers

;; FIXME: convert to generic
;; - functions => buffer targets
;; - current function / target

(defun nvp-makefile-target-name ()
  (save-excursion
    ;; forward one line so if point on target line
    ;; the target in the current line is toggled
    (forward-line 1)
    (makefile-previous-dependency)
    ;; `makefile-previous-dependency' modifies match-data
    ;; with `looking-at'
    (string-trim (match-string-no-properties 1))))

;; list dependencies for TARGET
(defun nvp-makefile-list-deps (target)
  (save-excursion
    (nvp-makefile-goto-target target)
    (skip-chars-forward ": \t" (point-at-eol))
    (split-string (buffer-substring-no-properties (point) (point-at-eol)))))

(defun nvp-makefile-list-targets ()
  (setq makefile-need-target-pickup t)
  (makefile-pickup-targets)
  makefile-target-table)

;; ------------------------------------------------------------
;;; Goto Locations

;; put point at end of matching target named TARGET
(defun nvp-makefile-goto-target (target)
  (let ((place (point)))
    (goto-char (point-min))
    (or (re-search-forward (concat "^" target) nil t)
        ;; if not found, put point back at start
        (and (goto-char place) nil))))

;; FIXME: unused
;; put point after current rule.  if in last rule, goto end of
;; buffer and insert newline if not at beginning of line
;; (defun nvp-makefile-goto-end-of-rule ()
;;   (or (makefile-next-dependency)
;;       (and (goto-char (point-max))
;;            (and (not (bolp))
;;                 (insert "\n")))))

;; -------------------------------------------------------------------
;;; Indent 

(defvar nvp-makefile-indent-offset 2)

;; indent ifeq ... endif regions
(defun nvp-makefile-indent ()
  (save-excursion
    (goto-char (point-min))
    ;; get first rule
    (let ((end (save-excursion
                 (progn (re-search-forward makefile-dependency-regex
                                           nil t)
                        (point)))))
      (while (search-forward "ifeq" end 'move)
        ;; indent if block
        (forward-line 1)
        (let ((close (save-excursion
                       (search-forward "endif")
                       (line-number-at-pos))))
          (while (< (line-number-at-pos) close)
            (beginning-of-line)
            (unless (looking-at-p "\\s-*else")
              (delete-horizontal-space)
              (indent-to nvp-makefile-indent-offset))
            (forward-line 1)))))))

;; ------------------------------------------------------------
;;; Hooks

;; cleanup buffer before save
(defun nvp-makefile-cleanup-buffer ()
  (unless (or buffer-read-only (not (buffer-modified-p)))
    ;; fixup indent
    (nvp-makefile-indent)
    ;; align [?:]= before first rule
    (align (point-min) (point-max))     ;use builtin align rules for now
    ;; (let ((end (save-excursion
    ;;              ;; find first rule
    ;;              (progn (goto-char (point-min))
    ;;                     (re-search-forward "^[^ ]+:" nil t)
    ;;                     (point)))))
    ;;   (align-regexp (point-min) end (nvp-concat
    ;;                                  "\\(?:[^<?]\\)\\(\\s-*\\)"
    ;;                                  "\\(=\\|[:?+]=\\)")
    ;;                 1))
    ;; align trailing '\'
    ;; (align-regexp (point-min) (point-max) "\\(\\s-*\\)\\\\\\s-*$")
    ))

(provide 'nvp-makefile)
;;; nvp-makefile.el ends here
