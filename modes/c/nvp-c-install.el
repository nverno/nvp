;;; nvp-c-install.el --- install -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/c-tools
;; Last modified: <2019-02-14 01:02:35>
;; Package-Requires: 
;; Created: 12 January 2019

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
(require 'nvp-c)
(declare-function nvp-log "nvp-log")

;; make includes.el and install dependencies or dont with NODEPS
;; force includes.el refresh with ARG
;;;###autoload
(defun nvp-c-install (arg &optional includes irony)
  (interactive "P")
  (let ((arg arg))
    (cond
      (includes 
       ;; write sys include paths to nvp-c-include.el
       (nvp-with-process-log (nvp-c-install-includes arg)
         :on-success (progn
                       (load (expand-file-name "nvp-c-include" (nvp-package-root)))
                       (nvp-c-install arg nil 'irony))))
      (irony
       ;; install irony server
       ;; do depends first, also only returns process object on windows
       ;; currently
       (if (not (require 'irony nil t))
           (nvp-log "Error: `irony' not installed")
         (nvp-with-gnu/w32
             (nvp-c-install-irony)
           (unless (file-exists-p irony-server-install-prefix)
             (nvp-c-install-irony irony-server-install-prefix)))))
      (t (nvp-c-install arg 'includes)))))

;;; Cache system include paths
;; regen includes after number of days or force with ARG
(defun nvp-c-install-includes (&optional arg)
  (let ((includes (expand-file-name "script/define-includes" (nvp-package-root))))
    (when (or (not (file-exists-p includes))
              (or arg (nvp-file-older-than-days includes 20)))
      (nvp-with-process "bash"
        :proc-name "define-includes"
        :proc-args (includes "make_sys_includes")))))

;;; Irony server

(nvp-with-gnu
  (defun nvp-c-install-irony ()
    (if (not (require 'irony nil t))
        (nvp-log "Error: `irony' not installed")
      (call-interactively 'irony-install-server))))

(nvp-with-w32
  ;; Install irony server using MSYS compilers. Return process object
  (defun nvp-c-install-irony (&optional irony-prefix irony-dir build-cmd)
    (let* ((irony-dir (or irony-dir
                          (expand-file-name "server"
                                            (file-name-directory
                                             (locate-library "irony")))))
           (build-dir (make-temp-file "_build" t))
           (irony-prefix (or irony-prefix
                             (expand-file-name ".emacs.d/cache/irony" "~")))
           (args (mapconcat 'identity
                            `(,irony-dir
                              "-G \"MSYS Makefiles\""
                              "-DCMAKE_CXX_COMPILER=g++.exe"
                              "-DCMAKE_C_COMPILER=gcc.exe"
                              ,(concat "-DCMAKE_INSTALL_PREFIX=" irony-prefix))
                            " "))
           (build-cmd
            "cmake --build . --use-stderr --config Release --target install")
           (default-directory build-dir))
      (start-process-shell-command
       "cmake" "*nvp-install*" (format "cmake %s && %s" args build-cmd)))))

(provide 'nvp-c-install)
;;; nvp-c-install.el ends here
