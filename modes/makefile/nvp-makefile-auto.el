;;; nvp-makefile-auto.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'make-mode)
(require 'nvp-makefile)


;; -------------------------------------------------------------------
;;; Goto targets

;; put point at end of matching target named TARGET
(defsubst nvp--makefile-goto-target (target)
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

(defmacro nvp-makefile-with-target (target &rest body)
  "Execute BODY with point after ':' following TARGET."
  (declare (indent defun) (debug (symbolp &rest form)))
  (nvp:with-gensyms (place)
    `(save-excursion
       ;; if target is found point will be at the end
       ;; of match, skip ahead to ':'
       (when (let ((,place (point)))
               (goto-char (point-min))
               (or (re-search-forward (concat "^" ,target) nil t)
                   ;; if not found, put point back at start
                   (and (goto-char ,place) nil)))
         (skip-chars-forward "^:" (line-end-position))
         (forward-char 1)
         ,@body))))

;; -------------------------------------------------------------------
;;; Used in snippets

(defun nvp-makefile--special-targets ()
  "Return list of makefile special targets."
  (let* ((buf (get-buffer-create " *special-targets*"))
         (bin (expand-file-name "bin/special-targets.py" nvp-makefile--dir))
         (stat (call-process bin nil (list buf nil) nil))
         (res (when (zerop stat)
                (with-current-buffer buf
                  (split-string (buffer-substring-no-properties (point-min) (point-max)))))))
    (prog1 res (kill-buffer buf))))

(defvar nvp-makefile-special-targets
  (eval-when-compile (ignore-errors (nvp-makefile--special-targets)))
  "List of special make targets.")

;;;###autoload(autoload 'nvp-makefile-special-targets "nvp-makefile-auto")
(nvp:define-cache-runonce nvp-makefile-special-targets ()
  "List of special make targets."
  (nvp-makefile--special-targets))

;; ------------------------------------------------------------
;;; Parse / Snippet helpers

;; FIXME: convert to generic
;; - functions => buffer targets
;; - current function / target

;; list dependencies for TARGET
(defun nvp-makefile-list-deps (target)
  (save-excursion
    (nvp--makefile-goto-target target)
    (skip-chars-forward ": \t" (line-end-position))
    (split-string (buffer-substring-no-properties (point) (line-end-position)))))

(defun nvp-makefile-list-targets ()
  (setq makefile-need-target-pickup t)
  (makefile-pickup-targets)
  makefile-target-table)

;; -------------------------------------------------------------------
;;; Auto add defines/targets/deps
;; used in snippets and toggles

;;; FIXME(6/25/24): find better way
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
  (save-buffer)
  (let ((target (nvp-makefile-target-name)))
    (when target
      (nvp-makefile-add-dep ".PHONY" target 'toggle))))

;; -------------------------------------------------------------------
;;; Snippets

;; FIXME: this is shitty
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
;;;###autoload
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
