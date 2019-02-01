;;; nvp-bookmark.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 24 November 2016

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
(require 'bookmark)

(declare-function bmk-to-bmk-handler "bmk-to-bmk")
(declare-function bmk-to-bmk-make-record "bmk-to-bmk")

;;;###autoload
(defun nvp-bookmark-local (file)
  (interactive
   (list (or
          (bound-and-true-p file)
          (and current-prefix-arg
               (read-file-name "Location of bookmark file: "))
          (and (bound-and-true-p local-bookmarks)
               (expand-file-name
                local-bookmarks
                (locate-dominating-file default-directory
                                        dir-locals-file)))
          bookmark-default-file)))
  (message "Current bookmarks file is: %s" file)
  (when (not (string= bookmark-default-file file))
    (bmk-to-bmk-handler (bmk-to-bmk-make-record file)))
  (call-interactively 'bookmark-bmenu-list))

(provide 'nvp-bookmark)
;;; nvp-bookmark.el ends here
