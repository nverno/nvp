;;; nvp-makefile-subrs.el --- compile-time -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(require 'nvp-macro)
(require 'nvp)
(nvp-decls :v (nvp-makefile-defun-regexp))

(defsubst nvp-makefile--defun-line-p ()
  (save-excursion
    (beginning-of-line 1)
    (looking-at-p nvp-makefile-defun-regexp)))

(defsubst nvp-makefile--skip-escapes (search-fn)
  (if (eq search-fn 're-search-backward)
      (eq (point-at-bol) (nvp-goto 'boll))
    (eq (point-at-eol) (nvp-goto 'eoll))))

;; preceded by '[^$]$[{(]'
(defsubst nvp-makefile-variable-or-function-p (pos)
  (and (memq (char-before pos) '(?{ ?\())
       (eq (char-before (1- pos)) ?$)
       (not (eq (char-before (- pos 2)) ?$))))

(defsubst nvp-makefile-rule-line-p (&optional pos)
  (save-excursion
    (and pos (goto-char pos))
    (beginning-of-line)
    (looking-at-p "^[^ \t\n]*:")))

;; -------------------------------------------------------------------
;;; Goto targets

;; put point at end of matching target named TARGET
(defsubst nvp-makefile-goto-target (target)
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
  (nvp-with-gensyms (place)
    `(save-excursion
       ;; if target is found point will be at the end
       ;; of match, skip ahead to ':'
       (when (let ((,place (point)))
               (goto-char (point-min))
               (or (re-search-forward (concat "^" ,target) nil t)
                   ;; if not found, put point back at start
                   (and (goto-char ,place) nil)))
         (skip-chars-forward "^:" (point-at-eol))
         (forward-char 1)
         ,@body))))


(provide 'nvp-makefile-subrs)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-makefile-subrs.el ends here
