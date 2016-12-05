;;; nvp-log --- 

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
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
(eval-when-compile
  (require 'nvp-macro))

(defvar nvp-log-buffer "*nvp-log*")

;;;###autoload
(defun nvp-log (text &optional buffer-name &rest args)
  (let ((buffer-name (or buffer-name
                         (bound-and-true-p nvp-log-buffer)))
        deactivate-mark)
    (with-current-buffer (get-buffer-create buffer-name)
      (nvp-log-mode)
      (goto-char (point-max))
      (insert-before-markers
       (apply 'format 
              (replace-regexp-in-string
               "\n+" "\n" (concat text "\n"))
              args)))))

;; ------------------------------------------------------------
;;; Mode

(require 'compile)

(defvar nvp-log-font-lock
  (eval-when-compile
    (let ((gstat (nvp-re-opt
                  '("finished" "deleted" "open" "success")'symbol))
          (wstat (nvp-re-opt '("warning") 'symbol))
          (rstat
           (nvp-re-opt
            '("exited abnormally" "failed" "error" "signal-description"
              "connection broken")
            'symbol))
          (gproc
           (nvp-re-opt '("run" "open" "listen" "connect") 'symbol))
          (rproc
           (nvp-re-opt
            '("stop" "exit" "signal" "closed" "failed") 'symbol)))
      `(("`\\([^\n']+\\)'" (1 font-lock-constant-face))
        (,gstat (1 'compilation-info))
        (,wstat (1 'compilation-warning))
        (,rstat (1 'compilation-line-number))
        (,gproc (1 'compilation-info))
        (,rproc (1 'compilation-line-number))))))

;;;###autoload
(define-derived-mode nvp-log-mode fundamental-mode "Log"
  (setq-local font-lock-defaults
              '(nvp-log-font-lock nil t nil nil)))

(provide 'nvp-log)
;;; nvp-log.el ends here
