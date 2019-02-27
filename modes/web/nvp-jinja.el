;;; nvp-jinja.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Created: 25 March 2018

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
  (require 'subr-x))

(declare-function web-mode-element-beginning "web-mode")
(declare-function web-mode-block-beginning "web-mode")

(autoload 'projectile-project-root "projectile")

;; true if in url_for clause and returns the file if it exists
;; TODO: expand to non-static?
;;;###autoload
(defun nvp-jinja-url-for ()
  (when-let* ((root (projectile-project-root))
              (regex (and (web-mode-block-beginning)
                          (looking-at-p "{{")
                          (re-search-forward
                           "\\(static\\).*'\\([^']+\\)'" (line-end-position))))
              (static (match-string 1))
              (file (expand-file-name (match-string 2)
                                      (expand-file-name static root))))
    (and (file-exists-p file) file)))

(provide 'nvp-jinja)
;;; nvp-jinja.el ends here
