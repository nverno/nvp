;;; sh-eldoc.el --- eldoc for bash/sh  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/shell-tools
;; Last modified: <2019-02-26 00:40:18>
;; Package-Requires: 
;; Created:  4 December 2016

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

;; Notes:
;; - help strings are cached
;; - bash builtins run synchronously
;; - others run async, so help will show up after moving point
;;   in region more than once

;;; Code:
(eval-when-compile
  (require 'nvp-macro))
(require 'nvp-sh-help)
(require 'eldoc)

(defvar sh-eldoc-cache (make-hash-table :test 'equal))

;; return formatted doc string for bash builtins
(defun sh-eldoc-builtin-string (cmd)
  (or (gethash cmd sh-eldoc-cache)
      (let ((str (nvp-sh-help-bash-builtin-sync cmd 'synopsis)))
        ;; remove 'cmd: ' and trailing newline
        (setq str (substring str (+ 2 (length cmd)) (1- (length str))))
        ;; propertize CMD
        (add-text-properties
         0 (length cmd)
         (list 'face 'font-lock-function-name-face) str)
        (puthash cmd str sh-eldoc-cache))))

;; get synopsis from man output asynchronously and cache it
(defun sh-eldoc--man (cmd)
  (sh-with-man-help cmd nil "*sh-eldoc*"
    (goto-char (point-min))
    (ignore-errors
      (when (search-forward "SYNOPSIS")
        (while (not (looking-at-p (concat "\\([ \t]+" cmd "\\|^[^ \t]\\)")))
          (forward-line))
        (skip-chars-forward " \t")
        ;; put result in cache
        (puthash
         cmd 
         (concat
          (propertize cmd 'face 'font-lock-function-name-face) ": "
          (and (looking-at "[^ \t]+[ \t]+\\([^\n]+\\)")
               (match-string 1))
          ;; (buffer-substring
          ;;  (+ (length cmd) (point)) (point-at-eol))
          )
         sh-eldoc-cache)
        (erase-buffer)))))

;; get doc string from man
(defun sh-eldoc-man-string (cmd)
  (or (gethash cmd sh-eldoc-cache)
      (ignore (sh-eldoc--man cmd))))

;;;###autoload
(defun sh-eldoc-function ()
  "Return eldoc string for bash functions (builtins and those avaliable \
from `man %s'."
  (let ((func (nvp-sh-help-current-command)))
    (and func
         (sh-with-bash/man func
           (sh-eldoc-builtin-string func) ;; synchronously
           (sh-eldoc-man-string func))))) ;; async

(provide 'sh-eldoc)
;;; sh-eldoc.el ends here
