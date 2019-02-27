;;; utop-inf ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/ocaml-tools
;; Package-Requires: 
;; Created:  9 December 2016

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
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'utop)

(defvar utop-inf-completions ())

(defsubst utop-inf-process ()
  (get-buffer-process utop-buffer-name))

;; call utop complete on STR, store completions in
;; `utop-inf-completions'
(defun utop-inf-redirect (str)
  (with-current-buffer utop-buffer-name
    (set-process-filter utop-process 'utop-inf-process-output)
    (utop-complete-input str)))

;; process utop output using `utop-inf-process-line'
;; when finished reset utops output filter
(defun utop-inf-process-output (_process output)
  (with-current-buffer utop-buffer-name
    (utop-perform
     (setq utop-output (concat utop-output output))
     (let ((lines (split-string utop-output "\n")))
       (while (>= (length lines) 2)
         (utop-inf-process-line (car lines))
         (setq lines (cdr lines)))
       (setq utop-output (car lines)))
     (set-process-filter utop-process 'utop-process-output))))

;; process utop output and push completions to `utop-inf-completions'
(defun utop-inf-process-line (line)
  (string-match "\\`\\([a-z-]*\\):\\(.*\\)\\'" line)
  (let ((command (match-string 1 line))
        (argument (match-string 2 line)))
    (cond
     ;; Complete with a word
     ((string= command "completion-word")
      (utop-set-state 'edit)
      (setq utop-inf-completions argument))
     ;; Start of completion
     ((string= command "completion-start")
      (setq utop-inf-completions nil))
     ;; A new possible completion
     ((string= command "completion")
      (push argument utop-inf-completions))
     ;; End of completion
     ((string= command "completion-stop")
      (utop-set-state 'edit)))))

(provide 'utop-inf)
;;; utop-inf.el ends here
