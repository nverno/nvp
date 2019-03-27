;;; nvp-makefile.el --- make helpers -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/make-tools
;; Last modified: <2019-03-27 04:45:38>
;; Created:  3 November 2016

;;; Commentary:

;; TODO:
;; - font-lock-doc-face for info/warning/error?
;; - incorporate semantic stuff?
;; - use info-completition-at-point function

;; FIXME:
;; - beginning/end of defun functions don't work
;; - yas expansions that add deps => commands
;; - collect remote info async
;; - align rules

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (nvp-local-vars)
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

;;-- Get values

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

;; -------------------------------------------------------------------
;;; Snippets

;; variables to set in snippet expansion environments
(defvar-local nvp-makefile-current-target nil)
(defvar-local nvp-makefile-yas-text nil)
(defvar yas-text)

;; set variables to use in snippet expansion
(defun nvp-makefile-yas-setenv (&optional targets)
  (when targets
    (setq nvp-makefile-current-target
          (nvp-completing-read "Add dependency to: " (nvp-makefile-list-targets)))))

(defun nvp-makefile-yas-text ()
  (setq nvp-makefile-yas-text yas-text)
  nil)

;; hook to run after snippet exits
(defun nvp-makefile-yas-exit-hook ()
  (when nvp-makefile-current-target
    (nvp-makefile-add-dep nvp-makefile-current-target nvp-makefile-yas-text)
    (setq nvp-makefile-current-target nil)))

;; ------------------------------------------------------------
;;; Goto Locations

;; put point at end of matching target named TARGET
(defun nvp-makefile-goto-target (target)
  (let ((place (point)))
    (goto-char (point-min))
    (or (re-search-forward (concat "^" target) nil t)
        ;; if not found, put point back at start
        (and (goto-char place) nil))))

;; put point after current rule.  if in last rule, goto end of
;; buffer and insert newline if not at beginning of line
(defun nvp-makefile-goto-end-of-rule ()
  (or (makefile-next-dependency)
      (and (goto-char (point-max))
           (and (not (bolp))
                (insert "\n")))))

;;-- Add stuff

;; add program ?= program to top if not already declared
(defun nvp-makefile-add-define (program &optional ifdef value)
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward (concat "^" program) nil t)
      (goto-char (point-min))
      (insert (format "%s %s= %s\n" program (if ifdef "?" "") (or value program))))))

(defun nvp-makefile-add-target (target)
  ;; annoying to try to track additions/deletions
  (setq makefile-need-target-pickup t)
  (makefile-pickup-targets)
  (save-excursion
    (unless (member target (mapcar 'car makefile-target-table))
     (forward-line 1)
     (makefile-previous-dependency)
     (insert (format "%s:\n" target)))))

;; add dependency for TARGET if not there
(defun nvp-makefile-add-dep (target dep &optional toggle delete)
  ;; ensure target exists, adding it if it doesn't
  (nvp-makefile-add-target target)
  (nvp-makefile-with-target target
    (let* ((deps (split-string
                  (buffer-substring-no-properties
                   (point) (point-at-eol))))
           (there (member dep deps)))
      (if (and there (or toggle delete))
          (progn
            (delete-region (point) (point-at-eol))
            (insert " ")
            (insert (mapconcat 'identity (delete dep deps) " ")))
        (when (not (or delete there))
          (delete-region (point) (point-at-eol))
          (insert " ")
          (insert (mapconcat 'identity
                             (nconc deps (cons dep nil)) " ")))))))

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

;; -------------------------------------------------------------------
;;; Toggle / Insert 

;; toggle this dependency to be an intermediate
(defun nvp-makefile-toggle-intermediate ()
  (interactive)
  (save-excursion
    (forward-line 1)
    (let* ((this (progn (makefile-previous-dependency)
                        (point)))
           (dep (string-trim (match-string-no-properties 1)))
           ;; (prev (progn (makefile-previous-dependency)
           ;;              (point)))
           )
      (if (looking-at-p ".INTERMEDIATE")
          (progn (kill-line)
                 (delete-char 1))
        ;; go back to current dependency
        (goto-char this)
        (insert (format ".INTERMEDIATE: %s\n" dep))))))

;; toggle current target as phony
(defun nvp-makefile-toggle-phony ()
  (interactive)
  (let ((target (nvp-makefile-target-name)))
    (when target
      (nvp-makefile-add-dep ".PHONY" target 'toggle))))

;;; Compile

(defun nvp-makefile-save-and-compile (&optional arg)
  "Save and compile.
With prefix ARG, run `helm-make'."
  (interactive "P")
  (save-buffer)
  (if arg (call-interactively 'helm-make)
    (call-interactively 'nvp-compile))
  (pop-to-buffer next-error-last-buffer))

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

;; -------------------------------------------------------------------
;;; Advice

(provide 'nvp-makefile)
;;; nvp-makefile.el ends here
