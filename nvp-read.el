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

;;--- netrc ----------------------------------------------------------

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

;; -------------------------------------------------------------------
;;; Man

;; make indentation based regexp
(defsubst nvp-read--man-indent ()
  (buffer-substring (point) (+ (point) (current-indentation))))

(defsubst nvp-read--man-indent-re ()
  (concat "^\\(?:[ \t]*$\\|" (nvp-read--man-indent) "\\)"))

(defsubst nvp-read--man-section-re ()
  (concat "^" (nvp-read--man-indent) "[^ \t\n\r]"))

;; regex to match man subentry
(defvar nvp-read--man-subentry-re
  "\\([^ \t]\\(?:[^ \t\n\r]\\| [^ \t\n\r]\\)+\\)")

;; return section from man doc
(defsubst nvp-read-man-string (section-re)
  (goto-char (point-min))
  (when (re-search-forward section-re nil 'move)
    ;; (forward-line)
    (beginning-of-line)
    (let* ((start (point))
           (section-re (nvp-read--man-section-re)))
      (forward-line)
      (while (not (or (eobp) (looking-at-p section-re)))
        (forward-line))
      (buffer-substring start (1- (point))))))

;; get switches from man section from START-RE to END-RE
(defsubst nvp-read-man-switches (section-re start-re end-re)
  (goto-char (point-min))
  (when (re-search-forward section-re)
    (forward-line)
    (let* ((indent-re (nvp-read--man-indent-re))
           (flag-re (concat indent-re nvp-read--man-subentry-re))
           (cont-re "\t[ \t]*\\|^$")
           res key start)
      (when (re-search-forward start-re)
        (beginning-of-line)
        (while (not (looking-at-p end-re))
          (if (not (looking-at flag-re))
              (forward-line)
            (setq key (match-string 1))
            ;; get description for key
            (setq start (match-end 0))
            (forward-line)
            (while (looking-at-p cont-re)
              (forward-line))
            (push
             (cons key
                   (replace-regexp-in-string
                    "^\\s-+\\|\t\\|\n$" ""
                    (buffer-substring start (1- (point)))))
             res))))
      (nreverse res))))

(provide 'nvp-read)
;;; nvp-read.el ends here
