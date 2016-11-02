;;; nvp-log --- 

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Copyright (C) 2016, Noah Peart, all rights reserved.
;; Created:  2 November 2016

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
(defvar nvp-log-buffer "*nvp*")

;;;###autoload
(defun nvp-log (text &optional buffer-name)
  (let ((buffer-name (or buffer-name
                         (bound-and-true-p nvp-log-buffer)))
        deactivate-mark)
    (with-current-buffer (get-buffer-create buffer-name)
      (goto-char (point-max))
      (insert-before-markers text)
      (insert "\n"))))

(provide 'nvp-log)
;;; nvp-log.el ends here
