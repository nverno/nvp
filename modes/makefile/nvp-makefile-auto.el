;;; nvp-makefile-auto.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-31.19>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 27 March 2019

;;; Commentary:

;; FIXME: gotta be a better way
;; - yas expansions that add deps => commands

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'nvp)
(require 'make-mode)


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

;; -------------------------------------------------------------------
;;; Auto add defines/targets/deps
;; used in snippets and toggles

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
;;; Toggle / Insert 

;; toggle this dependency to be an intermediate
;;;###autoload
(defun nvp-makefile-toggle-intermediate ()
  "Toggle current dependency as INTERMEDIATE."
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
;;;###autoload
(defun nvp-makefile-toggle-phony ()
  "Toggle current dependency as PHONY."
  (interactive)
  (let ((target (nvp-makefile-target-name)))
    (when target
      (nvp-makefile-add-dep ".PHONY" target 'toggle))))

;;; Compile
;;;###autoload
(defun nvp-makefile-save-and-compile (&optional arg)
  "Save and compile.
With prefix ARG, run `helm-make'."
  (interactive "P")
  (save-buffer)
  (if arg (call-interactively 'helm-make)
    (call-interactively 'nvp-compile))
  (pop-to-buffer next-error-last-buffer))

;; -------------------------------------------------------------------
;;; Snippets

;; FIXME: this is fucking shitty
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

(provide 'nvp-makefile-auto)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-makefile-auto.el ends here
