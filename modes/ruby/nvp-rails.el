;;; nvp-rails.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/ruby-tools
;; Last modified: <2019-01-25 23:38:31>
;; Package-Requires: 
;; Created: 31 December 2016

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

(defvar nvp-rails-buffer "*rails-tools*")
(defmacro nvp-rails-buffer ()
  `(with-current-buffer (get-buffer-create nvp-rails-buffer)
     (comint-mode)
     (current-buffer)))

;;;###autoload
(defun nvp-rails-server ()
  (interactive)
  (if (buffer-live-p nvp-rails-buffer)
      (display-buffer nvp-rails-buffer)
    (start-process-shell-command "rails" (nvp-rails-buffer)
                                 "rails server -e development")
    (display-buffer nvp-rails-buffer)
    (browse-url "http://localhost:3000")))

(provide 'nvp-rails)
;;; nvp-rails.el ends here
