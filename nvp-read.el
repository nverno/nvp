;;; nvp-read ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 29 November 2016

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

;; -------------------------------------------------------------------
;;; Netrc -- FIXME: outdated, all encrypted 

;; Regex to match machine name
(defvar nvp-read-netrc-machine-regex
  "\\(?:machine\\)\\s-+\\([^\n]+\\)")

;; Regex to match login and password.
(defvar nvp-read-netrc-login/pwd-regex
  (eval-when-compile
    (concat
     "\\(?:login\\)\\s-+\\([^\n]+\\)\n"
     "\\s-*\\(?:password\\)\\s-+\\([^\n]+\\)")))

;; Check .netrc file for login/password for machine.
;;;###autoload
(defun nvp-read-netrc (machine &optional location)
  (let ((netrc (or location
                   (expand-file-name ".netrc" "~")
                   (expand-file-name "_netrc" "~")))
        res)
    (if (file-exists-p netrc)
        (progn
          (with-temp-buffer
            (insert-file-contents netrc)
            (goto-char (point-min))
            (while (re-search-forward nvp-read-netrc-machine-regex
                                      nil t)
              (when (string= (match-string-no-properties 1) machine)
                (forward-line 1)
                (re-search-forward nvp-read-netrc-login/pwd-regex
                                   nil t)
                (when (and (match-string-no-properties 1)
                           (match-string-no-properties 2))
                  (setq res `(,(match-string-no-properties 1)
                              ,(match-string-no-properties 2))))))))
      (user-error "File %s doesn't exist." netrc))
    res))

(provide 'nvp-read)
;;; nvp-read.el ends here
