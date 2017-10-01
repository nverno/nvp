;;; nvp-comint ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 31 March 2017

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
(require 'comint)
(autoload 'nvp-inf-kill-proc-before-buffer "nvp-inf")

;; size of history files to save
(defvar nvp-comint-history-size 5000)

;;; History

;;;###autoload
(defun nvp-comint-setup-history (filename &optional size)
  ;; setup read/write for history file
  (setq comint-input-ring-file-name
        (expand-file-name filename nvp/cache))
  (setq-local comint-input-ring-size
              (or size nvp-comint-history-size))
  (comint-read-input-ring 'silent)
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (set-process-sentinel proc 'nvp-comint-history-sentinel)))
  ;; make sure the buffer exists before calling the process sentinel
  (add-hook 'kill-buffer-hook 'nvp-inf-kill-proc-before-buffer nil 'local))

(defun nvp-comint-history-sentinel (proc _m)
  (with-current-buffer (process-buffer proc)
    (comint-write-input-ring)))

;; comint redirect output silently (to temp buffer)
;;;###autoload
(defun nvp-comint-redirect-silently (proc string)
  (let (comint-redirect-perform-sanity-check)
    (with-temp-buffer
      (comint-redirect-send-command-to-process
       string (current-buffer) proc nil 'no-display)))
  (with-current-buffer (process-buffer proc)
    (comint-redirect-cleanup)))

(provide 'nvp-comint)
;;; nvp-comint.el ends here
