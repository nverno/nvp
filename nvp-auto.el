;;; nvp-auto.el --- lesser used autos -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-31 09:15:19>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 14 February 2019

;;; Commentary:
;; Autos rarely called
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro)
  (require 'hydra))
(require 'nvp)

;; -------------------------------------------------------------------
;;; Movement

;; jump to next char on this line. if matching char,
;; pressing the same key again jumps to the next one, etc.
;;;###autoload
(defun nvp-move-char-this-line (&optional char)
  (interactive (list (char-to-string (read-char "Char: " t))))
  (or (eq last-command this-command) (region-active-p) (push-mark))
  (let ((case-fold-search t))
    (condition-case nil
        (search-forward char (nvp-point 'eol))
      (error (let ((pt (point)))
               (beginning-of-line)
               (or (search-forward char (nvp-point 'eol))
                   (goto-char pt))))))
  (nvp-bind-transient-key
   char (lambda () (interactive)
          (setq this-command 'nvp-move-char-this-line)
          (nvp-move-char-this-line char))
   t))

;; used recursively below so not a macro
;;;###autoload
(defun nvp-bind-transient-key (key cmd &optional keep exit)
  "Bind KEY to CMD in transient map."
  (set-transient-map
   (let ((tmap (make-sparse-keymap)))
     (define-key tmap (kbd key) cmd)
     tmap)
   (or keep t)
   (or exit nil)))

;; see `paragraph-start' and `paragraph-separate' to extend
;;;###autoload
(defun nvp-move-forward-paragraph (&optional arg)
  (interactive "^p")
  (or arg (setq arg 1))
  (if (bolp)
      (progn
        (and (< arg 1) (forward-line -1))
        (forward-paragraph arg)
        (forward-line 1))
    (line-move arg)))

;;;###autoload
(defun nvp-move-backward-paragraph (&optional arg)
  (interactive "^p")
  (or arg (setq arg 1))
  (nvp-move-forward-paragraph (- arg)))

;; -------------------------------------------------------------------
;;; Duplicate lines 
(nvp-declare "" nvp-bind-transient-key)
(declare-function paredit-kill "paredit")

;; Duplicates the current line or region arg times.
;; if there's no region, the current line will be duplicated
;; (or last non-empty).
;;;###autoload
(defun nvp-duplicate-line-or-region (arg)
  (interactive "p")
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (nvp--duplicate-region arg beg end))
    (nvp--duplicate-last-nonempty-line arg)
    (nvp-use-transient-bindings nil :repeat-key "d")))

;; duplicate the current line num times.
(defun nvp-duplicate-current-line (&optional num)
  (interactive "p")
  (if (bound-and-true-p paredit-mode)
      (nvp--paredit-duplicate-current-line)
    (save-excursion
      (when (eq (nvp-point 'eol) (point-max))
        (goto-char (point-max))
        (newline)
        (forward-char -1))
      (nvp--duplicate-region num (nvp-point 'bol) (1+ (nvp-point 'eol))))))

(defun nvp--duplicate-back-and-dupe ()
  (interactive)
  (forward-line -1)
  (nvp-duplicate-current-line))

;; duplicates the region bounded by start and end num times.
;; if no start and end is provided, the current region-beginning and
;; region-end is used.
(defun nvp--duplicate-region (&optional num start end)
  (interactive "p")
  (save-excursion
    (let* ((start (or start (region-beginning)))
	   (end (or end (region-end)))
	   (region (buffer-substring start end)))
      (goto-char end)
      (dotimes (_i num)
	(insert region)))))

;; Duplicate the current of previous line that isn't blank.
(defun nvp--duplicate-last-nonempty-line (&optional num)
  (interactive "p")
  (let ((back 0))
    (while (and (save-excursion
                  (beginning-of-line)
                  (looking-at-p "[[:space:]]*$"))
                (> (line-number-at-pos) 1))
      (forward-line -1)
      (setq back (1+ back)))
    (when (eq (nvp-point 'eol) (point-max))
      (goto-char (point-max))
      (newline)
      (forward-char -1))
    (let ((region (buffer-substring (nvp-point 'bol) (1+ (nvp-point 'eol)))))
      (forward-line back)
      (dotimes (_i num)
        (insert region))))
  (goto-char (nvp-point 'eol)))

(defun nvp--paredit-duplicate-current-line ()
  (back-to-indentation)
  (let (kill-ring kill-ring-yank-pointer)
    (paredit-kill)
    (yank)
    (newline-and-indent)
    (yank)))

;; -------------------------------------------------------------------
;;; Paredit -- little used commands
(nvp-declare "paredit" paredit-delete-indentation)

;;;###autoload
(defun nvp-paredit-remove-newlines ()
  "Removes extra whitespace and newlines from current point to the next paren."
  (interactive)
  (let ((up-to (point)))
    (backward-char)
    (while (> (point) up-to)
      (nvp-paredit-delete-indentation))))

;; https://www.emacswiki.org/emacs/ParEdit
(defun nvp-paredit-delete-indentation (&optional arg)
  "Handle joining lines that end in a comment."
  (interactive "P")
  (let (comt)
    (save-excursion
      (move-beginning-of-line (if arg 1 0))
      (when (skip-syntax-forward "^<" (nvp-point 'eol))
        (setq comt (delete-and-extract-region (point) (nvp-point 'eol))))
      (delete-indentation arg)
      (when comt
        (save-excursion
          (move-end-of-line 1)
          (insert " ")
          (insert comt))))))

(provide 'nvp-auto)
;;; nvp-auto.el ends here
