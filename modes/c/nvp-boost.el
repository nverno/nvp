;;; nvp-boost. ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-12 20:43:25>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/c-tools
;; Package-Requires: 
;; Created: 18 January 2017

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
;;; Util

(defvar nvp-boost--version nil)

(eval-when-compile
  (defmacro nvp-boost--version-string (arg)
    `(or nvp-boost--version
         (setq nvp-boost--version
               (mapconcat
                'number-to-string
                (nvp-boost--version
                 (and ,arg (read-directory-name "Boost root directory: ")))
                "_")))))

;; get current boost version
(defun nvp-boost--version (&optional boost-root)
  (let ((file (expand-file-name "version.hpp" (or boost-root
                                                  (getenv "BOOST_ROOT")
                                                  "/usr/include/boost"))))
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (when (re-search-forward "#define BOOST_VERSION \\([0-9]+\\)")
          (let ((num (string-to-number (match-string 1))))
            (prog1 (list (/ num 100000) (% (/ num 100) 1000) (% num 100))
              (kill-buffer (current-buffer)))))))))

;; -------------------------------------------------------------------
;;; Commands

;;;###autoload
(defun nvp-boost-version (arg)
  (interactive "P")
  (message "Boost version: %s" (nvp-boost--version-string arg)))

;;;###autoload
(defun nvp-boost-lookup-help (arg)
  (interactive "P")
  (browse-url (concat "www.boost.org/doc/libs/" (nvp-boost--version-string arg))))

(provide 'nvp-boost)
;;; nvp-boost.el ends here
