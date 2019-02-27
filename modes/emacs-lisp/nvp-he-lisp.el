;;; nvp-he-lisp.el --- lisp hippie expansion -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/elisp-utils
;; Last modified: <2019-01-15 00:04:48>
;; Package-Requires: 
;; Created: 19 March 2017

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

;; Fuzzy expansion for hyphen-separated commands, eg. i-p => in-package

;;; Code:
(eval-when-compile
  (require 'cl-lib))
(require 'hippie-exp)

;; collect matches for REGEXP. If BUFFER is non-nil, collect matches from
;; BUFFER (default current buffer)
(defun nvp-he-lisp-matches (regexp &optional buffer)
  (and buffer (set-buffer buffer))
  (let (res)
   (save-excursion
     (goto-char (point-min))
     (while (re-search-forward regexp nil 'move)
       (push (match-string 0) res)))
   res))

;; create regexp from STR matching expansions around hypens, eg
;; r-r => "\\br\\w*-r[A-Za-z0-9-]*\\b"
;; so it matches replace-regexp-in-string, for example
(defsubst nvp-he-lisp-regexpify (str)
  (concat "\\b" (replace-regexp-in-string "-" "\\\\w*-" str) "[A-Za-z0-9-]*\\b"))

;;;###autoload
(defun nvp-he-try-expand-flex-lisp (old)
  "Try to complete lisp symbol from current buffer, using fuzzy matching around
'-' separators, eg. \"i-p\" => \"in-package\"."
  (unless old
    ;; possibly, slime-symbol-start-pos, slime-symbol-end-pos
    (he-init-string (he-lisp-symbol-beg) (point))
    (unless (he-string-member he-search-string he-tried-table)
      (setq he-tried-table (cons he-search-string he-tried-table)))
    (setq he-expand-list                ;build expansion list
          (and (not (equal "" he-search-string))
               (nvp-he-lisp-matches (nvp-he-lisp-regexpify he-search-string)))))
  (while (and he-expand-list            ;remove candidates already found
              (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (prog1 (not (null he-expand-list))    ;return t if expansion is found
    (if (not he-expand-list)
        (and old (he-reset-string))
      (he-substitute-string (pop he-expand-list)))))

(provide 'nvp-he-lisp)
;;; nvp-he-lisp.el ends here