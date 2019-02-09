;;; nvp-help-auto.el --- help commands -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-09 08:50:25>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Maintainer: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created:  2 February 2019

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
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'nvp-macro))
(require 'help-fns)
(nvp-declare "define-word" define-word define-word-at-point)
(autoload 'ispell-get-word "ispell")

;;; TODO:
;; - list package dependencies: see `package--get-deps'
;; - describe keymap

;; -------------------------------------------------------------------
;;; Lookup Words

;; Define word at point, with single prefix prompt for word, 
;; with two prefix use lookup-word.
;;;###autoload
(defun nvp-help-define-word (arg)
  "Define word at point, dispatching based on ARG."
  (interactive "p")
  (cond
   ((eq arg 4) (call-interactively  #'define-word))
   ((eq arg 16) (call-interactively #'nvp-help-lookup-word))
   (t (call-interactively           #'define-word-at-point))))

;; Lookup definintion of word at point online.
(defun nvp-help-lookup-word (word)
  (interactive (list (save-excursion (car (ispell-get-word nil)))))
  (browse-url (format "http://en.wiktionary.org/wiki/%s" word)))

;; -------------------------------------------------------------------
;;; Faces 

;; Show the name of face under point.
;;;###autoload
(defun nvp-help-font-face (pos)
  "Info on char face at POS."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face)
      (message "No face at %d" pos))))

;; -------------------------------------------------------------------
;;; Charsets
 
;;;###autoload
(defun nvp-help-list-charsets (&optional arg)
  "List names of charsets."
  (interactive "P")
  (if arg
      (list-character-sets nil)
    (let* ((table (mapcar (lambda (x) (list (symbol-name x))) charset-list))
           (charset (ido-completing-read "Charset: " table)))
     (list-charset-chars (intern charset)))))

;;;###autoload
(defun nvp-help-ascii-table ()
  "Print ASCII table values."
  (interactive)
  (nvp-with-results-buffer "*ASCII*"
    (insert "              ASCII               \n"
            "----------------------------------\n")
    (cl-loop
       for i from 0 to 15
       do (cl-loop
             for j from 0 to 15
             as num = (+ i (* j 16))
             do
               (when (= j 0) (insert (format "%4d |" i)))
               (insert (format " %c " num))
               (when (= j 15) (insert "\n"))))))

;; -------------------------------------------------------------------
;;; Bindings

;;;###autoload
(defun nvp-help-describe-bindings (prefix)
  "Describe bindings beginning with PREFIX."
  (interactive (list (read-string "Bindings prefix (enter as for 'kbd'): ")))
  (describe-bindings (kbd prefix)))

;;;###autoload
(defun nvp-help-describe-keymap (keymap)
  "Describe KEYMAP readably."
  (interactive
   ;; #<marker at 34938 in help-fns.el.gz>
   (let ((v (variable-at-point))
         (enable-recursive-minibuffers t)
         (orig-buffer (current-buffer))
         val)
     (setq val (completing-read
                (if (and (symbolp v) (keymapp v))
                    (format "Describe keymap (default %s): " v)
                  "Describe keymap: ")
                #'help--symbol-completion-table
                (lambda (vv)
                  (with-current-buffer orig-buffer
                    (or (get vv 'variable-documentation)
                        (and (boundp vv) (keymapp vv)))))
                t nil nil
                (if (symbolp v) (symbol-name v))))
     (list (if (equal val "") v (intern val)))))
  (cl-assert (keymapp keymap))
  (setq keymap (or (ignore-errors (indirect-variable keymap)) keymap))
  (help-setup-xref (list #'nvp-help-describe-keymap keymap)
                   (called-interactively-p 'interactive))
  (let ((name (symbol-name keymap))
        (doc (documentation-property keymap 'variable-documentation)))
    (and (equal "" doc) (setq doc nil))
    (with-help-window (help-buffer)
      (princ name) (terpri) (princ (make-string (length name) ?-)) (terpri) (terpri)
      (with-current-buffer standard-output
        (when doc
          (princ doc) (terpri) (terpri))
        ;; see https://www.emacswiki.org/emacs/help-fns%2b.el
        ;; Use `insert' instead of `princ', so control chars (e.g. \377) insert
        ;; correctly.
        (insert (substitute-command-keys (concat "\\{" name "}")))))))

;;; https://www.emacswiki.org/emacs/help-fns%2b.el
;; describe-package ?

(provide 'nvp-help-auto)
;;; nvp-help-auto.el ends here
