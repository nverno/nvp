;;; nvp-info.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-07 08:13:04>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Maintainer: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 31 January 2019

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
  (require 'cl-lib)
  (nvp-local-vars)
  (defvar nvp-info-nodes))
(require 'info)

;;;###autoload
(defun nvp-info-open (topic &optional bname)
  "Open info on TOPIC in BNAME."
  (interactive (list (ido-completing-read "Topic: " nvp-info-nodes)))
  (let ((buff (or bname (concat "*" topic " info*"))))
    (if (get-buffer buff)
        (progn
          (switch-to-buffer buff)
          (unless (string-match topic Info-current-file)
            (Info-goto-node (format "(%s)" topic))))
      (info topic buff))))

;;;###autoload
(defun nvp-info-install (file)
  "Install FILE into info directory."
  (interactive
   (list (ido-read-file-name "File: " (expand-file-name "org" nvp/info))))
  (let ((default-directory (expand-file-name "org" nvp/info))
        (target
         (concat "install-"
                 (file-name-nondirectory (file-name-sans-extension file)))))
    (nvp-with-process "make"
      :proc-name "install-info"
      :proc-args (target))))

(provide 'nvp-info)
;;; nvp-info.el ends here
