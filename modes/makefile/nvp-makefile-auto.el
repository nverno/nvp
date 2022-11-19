;;; nvp-makefile-auto.el ---  -*- lexical-binding: t; -*-

;;; Commentary:

;; FIXME: gotta be a better way
;; - yas expansions that add deps => commands

;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'make-mode)
(nvp:req 'nvp-makefile 'subrs)
(require 'nvp)

;; -------------------------------------------------------------------
;;; Used in snippets

;; Special targets: collect matches from url
(defun nvp-makefile-collect-topics (url regex)
  (let (res)
    (nvp:while-scanning-url url regex
      (push (match-string-no-properties 1) res))
    res))

;;;###autoload(autoload 'nvp-makefile-special-targets "nvp-makefile-auto")
(nvp:define-cache-runonce nvp-makefile-special-targets ()
  "List of special make targets."
  ;; propertize :manual (concat url (match-string 1))
  (nvp-makefile-collect-topics
   "https://www.gnu.org/software/make/manual/html_node/Special-Targets.html"
   "dt[>< ]+code[<> ]+\\([.A-Za-z]+\\)"))

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
    (skip-chars-forward ": \t" (line-end-position))
    (split-string (buffer-substring-no-properties (point) (line-end-position)))))

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
                   (point) (line-end-position))))
           (there (member dep deps)))
      (if (and there (or toggle delete))
          (progn
            (delete-region (point) (line-end-position))
            (insert " ")
            (insert (mapconcat 'identity (delete dep deps) " ")))
        (when (not (or delete there))
          (delete-region (point) (line-end-position))
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
