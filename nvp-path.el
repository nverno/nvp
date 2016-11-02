;;; nvp-path --- 

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Copyright (C) 2016, Noah Peart, all rights reserved.
;; Created:  1 November 2016

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
  (require 'cl-lib))
(require 'nvp)
(require 'nvp-log)

;; ------------------------------------------------------------

;;; Environment Variables

;; Append DIR to environment variable PATH and EXEC-PATH.
;;;###autoload
(defun nvp-path-exec-add (dir)
  (let ((path (cl-remove-duplicates
               (cons dir (split-string (getenv "PATH") path-separator))
               :test (lambda (x y)
                       (if (eq system-type 'windows-nt)
                           (string= (downcase x) (downcase y))
                         (string= x y))))))
    (setenv "PATH" (mapconcat 'identity path path-separator))
    (setq exec-path path)))

;; update registry value on windows
(defun nvp-path-w32-setenv! (env-var value &optional exec clobber)
  (let* ((ps (expand-file-name "tools/Set-Env.ps1" nvp--dir))
         (val (replace-regexp-in-string "/" "\\\\" value)))
    (w32-shell-execute "runas" "powershell"
                       (format " -File %s \"%s\" \"%s\" %s"
                               ps env-var val (if clobber "-Clobber" "")))
    ;; update env for current session as well
    (when exec
      (if (string= (upcase env-var) "PATH")
          (nvp-path-exec-add val)
        (setenv env-var val)))))

;; Update registry value ENV-VAR by setting/appending VALUE (user).
;; add to exec-path if EXEC is non-nil. If CLOBBER is non-nil,
;; overwrite variable if it already exists.
;;;###autoload
(defun nvp-path-setenv! (env-var value &optional exec clobber)
  (cond
   ((eq system-type 'windows-nt)
    (nvp-path-w32-setenv! env-var value exec clobber))
   (t (user-error "FIXME"))))

;;; Search Path

;; update executable database, rerun build script
;; defaults to $APPDATA/exe-index on windows
;;;###autoload
(defun nvp-path-update-path-db (&optional path)
  (nvp-log "Updating executable database")
  (cond
   ((eq system-type 'windows-nt)
    (set-process-sentinel
     (apply #'start-process
            "updatedb" nvp-log-buffer "powershell"
            `("-f" ,(expand-file-name "Update-ExecDB.ps1" nvp/binw)
              ,@path))
     #'nvp-path-update-path-db-sentinel))))

(defun nvp-path-update-path-db-sentinel (p m)
  (nvp-log (format "%s: %s" (process-name p) m))
  (when (zerop (process-exit-status p))
    (nvp-log "DB Update completed")))

(provide 'nvp-path)
;;; nvp-path.el ends here
