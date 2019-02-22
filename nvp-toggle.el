;;; nvp-toggle.el --- toggle/insert stuff  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-22 01:47:25>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 20 March 2017

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;; - see repeat.el for repeating last command with last input
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (require 'time-stamp))
(require 'files-x)
(declare-function time-stamp "time-stamp")

;;; FIXME:
;; (defun nvp-toggle--repeat (arg)
;;   (and (consp arg) (setq arg (car arg)))
;;   (let ((repeat-key last-input-event))
;;     (unless (current-message)
;;       (message "(Type %s to repeat %s)"
;;                (format-kbd-macro (vector repeat-key))
;;                ()))))

;;;###autoload
(defun nvp-toggle-timestamp (&optional arg)
  "Insert/update timestamp for current buffer."
  (interactive "P")
  (let ((time-stamp-active t)
        (time-stamp-count (if arg (read-number "Max time stamps: ") 1))
        (time-stamp-pattern (or time-stamp-pattern
                                (pcase major-mode
                                  (`org-mode "#\\+DATE: <%%>$")
                                  (_ "15/Last modified: <%%>$")))))
    (time-stamp)))

;; In/De-crement numbers in region,  decremnent with prefix argument
;;;###autoload
(defun nvp-toggle-increment-numbers (&optional bnds inc)
  "Simple function to increment numbers in region. Decrement with prefix.
Call repeatedly with 'i'."
  (interactive
   (list
    (nvp-region-or-batp
      (if (eq 16 (car current-prefix-arg))
          (intern (read-from-minibuffer "Thingatpt (default 'paragraph): "
                                        nil nil nil nil 'paragraph))
        'paragraph))
    (if (eq 4 (car current-prefix-arg)) -1 1)))
  (unless bnds
    (user-error "No region to search in."))
  (let (deactivate-mark)
    (nvp-regex-map-all-matches          ;first de/increment
     (lambda (ms)
       (replace-match (number-to-string (+ inc (string-to-number ms)))))
     "\\([-]?[[:digit:]]+\\)" bnds nil 1)
    ;; repeating the command
    (if (null nvp-prefix-arg)
        (message "Repeating: %S %S" nvp-prefix-arg ))
    (setq prefix-arg current-prefix-arg)
    (set-transient-map
     (let ((km (make-composed-keymap `(,(make-sparse-keymap) universal-argument-map)
                                     universal-argument-map)))
       ;; (define-key km (kbd "C-u") #'universal-argument)
       ;; (define-key km (kbd "C-u C-u") #'universal-argument-more)
       (define-key km (vector last-command-event) this-command)
       (define-key km "d" (lambda ()
                            (interactive)
                            (setq current-prefix-arg '(4))
                            (call-interactively 'nvp-toggle-increment-numbers)))
       km)
     t)))

;; -------------------------------------------------------------------
;;; Toggle file/directory local variables

;; remove empty prop-line -- point should be in -*- -*-
(defun nvp-toggle--cleanup-prop-line ()
  "Remove empty prop-line."
  (when (and (looking-at-p "[ \t]*-\\*-")
             (looking-back "-\\*-[ \t]*" (line-beginning-position)))
    (skip-chars-backward " \t\\*-" (line-beginning-position))
    (delete-region (point) (line-end-position))))

;; get value corresponding to variable which may be shortened, eg. `mode'
(defun nvp-toggle--normalize-var (var)
  (pcase var
    (`mode major-mode)
    (`coding buffer-file-coding-system)
    (_ (if (and (symbolp var)
                (boundp var))
           (symbol-value var)))))

;; normalize possible shortened mode names
(defun nvp-toggle--normalize-mode (val)
  (let ((str (if (stringp val) val (symbol-name val))))
    (if (string-match-p "-mode\\'" str)
        val
      (intern (concat str "-mode")))))

;;;###autoload
(defun nvp-toggle-local-variable (var &optional val dir footer)
  "Toggle file/dir-local binding of VAR with VAL.
If DIR is non-nil toggle dir-local variable.
If FOOTER is non-nil toggle value in file's Local Variables."
  (and (stringp var) (setq var (intern var)))
  (let* ((curr (nvp-toggle--normalize-var var))
         (op (if (or (not curr)
                     (and val (not (equal curr (if (eq var 'mode)
                                                   (nvp-toggle--normalize-mode val)
                                                 val)))))
                 'add-or-replace
               'delete))
         (val (and (not (eq op 'delete)) (or val t)))
         (prop (or (member var '(mode lexical-binding))
                   (not (or dir footer))))
         (fn (cond
              (prop #'modify-file-local-variable-prop-line)
              (dir  #'modify-dir-local-variable)
              (t    #'modify-file-local-variable)))
         (dir-mode (and (not prop) dir (read-file-local-variable-mode)))
         (start-state (buffer-chars-modified-tick))
         changed)
    (let ((orig-buff (current-buffer)))
      (save-excursion                   ;modify-file... moves point
        (apply fn (if dir-mode (list dir-mode var val op) (list var val op)))
        (when (and prop (eq op 'delete))
          (nvp-toggle--cleanup-prop-line)))
      (setq changed (not (or (eq fn 'modify-dir-local-variable)
                             (eq start-state (buffer-chars-modified-tick)))))
      ;; save/revert buffer if changes were made
      (and changed (save-buffer))
      (if (not (or changed (eq fn 'modify-dir-local-variable)))
          (message "No changes local variables changed.")
        (with-current-buffer orig-buff ;dir-local modification changes buffer
          (revert-buffer 'ignore-auto 'no-confirm))
        (message "%s %s%s in the %s"
                 (if (eq op 'delete) "Deleted" "Updated")
                 (symbol-name var)
                 (if val (concat " => " (if (stringp val) val (symbol-name val))) "")
                 (if prop "prop-line" (if dir "dir-locals" "local vars")))))))

;;;###autoload
(defun nvp-toggle-file-local-binding (&optional footer)
  "Toggle file local binding.
If FOOTER is non-nil, use Local Variable list, otherwise -*- line."
  (interactive "P")
  (let ((var (read-file-local-variable "Toggle file-local variable")))
    (nvp-toggle-local-variable
     var (read-file-local-variable-value var) nil footer)))

;;;###autoload
(defun nvp-toggle-dir-local-binding (var val)
  "Toggle dir-local binding of VAR to VAL."
  (interactive
   (let ((var (read-file-local-variable "Add dir-local variable")))
     (list var (read-file-local-variable-value var))))
  (nvp-toggle-local-variable var val 'dir))

(provide 'nvp-toggle)
;;; nvp-toggle.el ends here
