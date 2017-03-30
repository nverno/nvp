;;; nvp-session ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 30 March 2017

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
  (nvp-local-vars)
  (require 'cl-lib))
(require 'desktop)

(defsubst nvp-session--read (prompt)
  (ido-completing-read
   prompt
   (directory-files (expand-file-name "sessions" nvp/cache) t "^[.][^.]")))

;;;###autoload
(defun nvp-session-save (&optional name)
  (interactive (list (nvp-session--read "Save session: ")))
  (desktop-save name))

;;;###autoload
(defun nvp-session-load (&optional name)
  (interactive (list (nvp-session--read "Load session: ")))
  (desktop-read name))

(provide 'nvp-session)
;;; nvp-session.el ends here
