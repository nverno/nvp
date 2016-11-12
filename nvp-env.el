;;; nvp-env --- 

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
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
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'nvp)
(require 'nvp-log)

;; ------------------------------------------------------------
;;; PATH

;; Append DIR to environment variable PATH and EXEC-PATH.
;;;###autoload
(defun nvp-env-exec-add (dir)
  (let ((path (cl-remove-duplicates
               (cons dir (split-string (getenv "PATH") path-separator))
               :test (lambda (x y)
                       (nvp-with-gnu/w32
                           (string= x y)
                         (string= (downcase x) (downcase y)))))))
    (setenv "PATH" (mapconcat 'identity path path-separator))
    (setq exec-path path)))

;; move path entries matching regex to front of path and return
;; new path, preserve existing order of entries otherwise
(defun nvp-env-rearrange-path (regex &optional case-fold)
  (let ((parts (split-string (getenv "PATH") path-separator))
        (case-fold-search case-fold)
        hits misses)
    (mapc (lambda (s)
            (if (string-match-p regex s)
                (push s hits)
              (push s misses)))
          parts)
    (nconc (nreverse hits) (nreverse misses))))

;; rearrange process environmen path and return process-environment
;; with new value tacked onto front (first gets used)
(defun nvp-env-rearrange-process-path (regex &optional case-fold)
  (cons (concat "PATH="
                (mapconcat 'identity
                           (nvp-env-rearrange-path regex case-fold)
                           path-separator))
        process-environment))

;; ------------------------------------------------------------
;;; Setenv

;;;###autoload (autoload 'nvp-env-setenv! "nvp-env")
(nvp-with-w32
  ;; Update registry value ENV-VAR by setting/appending VALUE (user).
  ;; add to exec-path if EXEC is non-nil. If CLOBBER is non-nil,
  ;; overwrite variable if it already exists.
  (defun nvp-env-setenv! (env-var value &optional exec clobber)
    (let* ((ps (expand-file-name "tools/Set-Env.ps1" nvp--dir))
           (val (replace-regexp-in-string "/" "\\\\" value)))
      (w32-shell-execute "runas" "powershell"
                         (format " -File %s \"%s\" \"%s\" %s"
                                 ps env-var val (if clobber "-Clobber" "")))
      ;; update env for current session as well
      (when exec
        (if (string= (upcase env-var) "PATH")
            (nvp-env-exec-add val)
          (setenv env-var val))))))

(nvp-with-gnu
  (defun nvp-env-setenv! (env-var value &optional exec clobber)
    ;; FIXME: todo
    (user-error "TODO")))

;; ------------------------------------------------------------
;;; Search Path

;;;###autoload (autoload 'nvp-env-update-path-db "nvp-env")
(nvp-with-w32
  ;; update executable database, rerun build script
  ;; defaults to $APPDATA/exe-index on windows
  (defun nvp-env-update-path-db (&optional path)
    (nvp-log "Updating executable database")
    (cond
     ((eq system-type 'windows-nt)
      (set-process-sentinel
       (apply #'start-process
              "updatedb" nvp-log-buffer "powershell"
              `("-f" ,(expand-file-name "Update-ExecDB.ps1" nvp/binw)
                ,@path))
       #'nvp-env-update-path-db-sentinel))))

  (defun nvp-env-update-path-db-sentinel (p m)
    (nvp-log (format "%s: %s" (process-name p) m))
    (when (zerop (process-exit-status p))
      (nvp-log "DB Update completed"))))

;; ------------------------------------------------------------

(declare-function w32-shell-execute "w32")

(provide 'nvp-env)
;;; nvp-env.el ends here
