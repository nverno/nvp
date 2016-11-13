;;; nvp-ext --- -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/
;; Package-Requires: 
;; Created: 11 November 2016

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
  (require 'nvp-macro))

(defmacro nvp-ext-read-passwd ()
  '(or (bound-and-true-p nvp-sudo-passwd)
       (read-passwd "Password: ")))

(nvp-with-gnu
  ;; do sudo command and return process object
  (defun nvp-ext-sudo-command (&optional password command buffer)
    (interactive)
    (let* ((password (or password (nvp-ext-read-passwd)))
           (cmd (or command (read-shell-command "Command: ")))
           (proc (start-process-shell-command
                  "bash" (or buffer "*nvp-install*")
                  (concat "sudo bash " cmd))))
      (process-send-string proc password)
      (process-send-string proc "\r")
      (process-send-eof proc)
      proc))

  (defun nvp-ext-sudo-install (packages &optional buffer)
    (interactive (list (read-shell-command "Packages: ")))
    (nvp-ext-sudo-command
     nil
     (format "-l -c \"apt-get install -y %s\"" packages)
     buffer)))

(provide 'nvp-ext)
;;; nvp-ext.el ends here
