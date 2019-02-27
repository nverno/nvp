;;; hippie-expand-nrepl.el --- hippie expand nrepl history -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/clojure-tools
;; Package-Requires: 
;; Created:  6 January 2017

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
  (require 'cl-lib))
(require 'hippie-exp)
(require 'cider-repl)

(defvar-local he-nrepl--index nil)
(defvar-local he-nrepl--matches ())

;; return point at beginning of prompt
(defsubst he-nrepl-bol ()
  (marker-position cider-repl-input-start-mark))

;; if expansion starts with '(', remove trailing ')', paredit will do rest
(defsubst he-repl-expand (str)
  (and str
       (if (and (string= "(" (substring str 0 1))
                (string= ")" (substring str -1)))
           (substring str 0 -1)
         str)))

;; Hippie expansion from nrepl history
;; OLD must be nil on first call to function, and t for  successive expansions.
;;;###autoload
(defun try-expand-nrepl-history (old)
  (and cider-repl-input-history
       (let (expansion)
         (unless old
           (he-init-string (he-nrepl-bol) (point))
           (setq he-nrepl--index 0)
           (setq he-nrepl--matches
                 (and (not (equal "" he-search-string))
                      (cl-remove-duplicates
                       (all-completions
                        he-search-string
                        cider-repl-input-history)
                       :test 'string=
                       :from-end t))))
         (when he-nrepl--matches
           (setq expansion (he-repl-expand
                            (nth he-nrepl--index he-nrepl--matches)))
           (setq he-nrepl--index (1+ he-nrepl--index)))
         (if (not expansion)
             (ignore (and old (he-reset-string)))
           (he-substitute-string expansion t)))))

;;;###autoload
(defun hippie-expand-nrepl-setup ()
  (interactive)
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-expand-nrepl-history))

(provide 'hippie-expand-nrepl)
;;; hippie-expand-nrepl.el ends here
