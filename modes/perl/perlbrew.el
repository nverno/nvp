;;; perlbrew.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/perl-tools
;; Package-Requires: 
;; Created:  2 January 2017

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
(autoload 'string-trim "subr-x")

(eval-and-compile
  (defvar perlbrew-root (or (getenv "PERLBREW_ROOT")
                            (expand-file-name "perl5/perlbrew" (getenv "HOME"))))

  (defvar perlbrew-exe
    (and (bound-and-true-p perlbrew-root)
         (expand-file-name "bin/perlbrew" perlbrew-root))))

;; -------------------------------------------------------------------
;;; Utils

(eval-when-compile
  (defmacro perlbrew--listify (args)
    "Convert ARGS to list if it a string."
    `(if (stringp ,args) `(cons ,args nil) ,args))

  ;; call perlbrew shell command on ARGS, return string or list of strings
  (defmacro perlbrew--call (args &optional lines)
    "Call perlbrew with ARGS and return response as string, optionally as 
list of strings (`process-lines')."
    (let ((args_ (cl-gensym)))
      `(let ((,args_ ,(perlbrew--listify args)))
         ,(if lines
              `(mapcar 'string-trim (apply 'process-lines ,perlbrew-exe ,args_))
            `(string-trim
              (shell-command-to-string
               (mapconcat 'identity (cons ,perlbrew-exe ,args_) " ")))))))

  (defmacro perlbrew--execute (args &optional async &rest body)
    "Call perlbrew with ARGS, optionally do asynchronously and do BODY when
process finishes successfully."
    (let ((args_ (cl-gensym)))
      `(let ((,args_ ,(perlbrew--listify args)))
         ,(if async
              `(set-process-sentinel
                (apply 'start-process "perlbrew" nil ,perlbrew-exe ,args_)
                #'(lambda (p m)
                    (if (not (zerop (process-exit-status p)))
                        (user-error "%s: %m" (process-name p) m)
                      ,@body)))
            `(apply 'call-process ,perlbrew-exe nil nil nil ,args_)))))

  ;; list available perls + system
  (defmacro perlbrew--list ()
    "List available perls."
    `(let* ((perls (let (r)
                     (mapc
                      (lambda (perl)
                        (and (string-match "^[* \t]*\\(\\(?:perl\\|[0-9]\\).*\\)" perl)
                             (push (match-string 1 perl) r)))
                      (perlbrew--call "list" 'lines))
                     r)))
       `(,@perls "system")))

  ;; completing read for perl version
  (defmacro perlbrew--read ()
    "Completing read for perlbrew perls (+ system)."
    `(ido-completing-read "Perl version: " (perlbrew--list) nil t))

  ;; remove old perls from path/exec-path, optionally add new one
  (defmacro perlbrew--update-path (&optional new-version)
    "Update PATH and EXEC-PATH, removing perlbrew perls and optionally
replacing with NEW-VERSION."
    (let ((prefix (expand-file-name "perls" perlbrew-root)))
      `(let ((path
              (cl-remove-if
               (lambda (s) (string-prefix-p ,prefix s))
               (split-string (getenv "PATH") path-separator))))
         (when ,new-version
           (setq path (cons (expand-file-name (concat "perls/" ,new-version
                                                      "bin/perl")
                                              ,perlbrew-root)
                            path)))
         (setq exec-path path)
         (setenv "PATH" (mapconcat 'identity path path-separator)))))

  (defmacro perlbrew--msg (str &rest args)
    `(message (concat "[perlbrew]: " ,str) ,@args))

  (defmacro perlbrew--sys/version (version sys-call ver-call)
    "If VERSION is 'system' do SYS-CALL, otherwise do VER-CALL."
    `(if (string= ,version "system")
         (progn
           (perlbrew--update-path)
           (perlbrew--execute ,sys-call)
           (perlbrew--msg "using system perl"))
       (perlbrew--update-path ,version)
       (perlbrew--execute ,ver-call)
       (perlbrew--msg "using %s" ,version))))

;; -------------------------------------------------------------------
;;; Commands

;;;###autoload
(defun perlbrew (args)
  "Call perlbrew with ARGS."
  (interactive "M$ perlbrew ")
  (perlbrew--call args))

;;;###autoload
(defun perlbrew-use (version)
  "Use perl version."
  (interactive (list (perlbrew--read)))
  (perlbrew--sys/version version "off" (list "use" version)))

;;;###autoload
(defun perlbrew-switch (version)
  "Switch perl version."
  (interactive (list (perlbrew--read)))
  (perlbrew--sys/version version "switch-off" (list "switch" version)))

(provide 'perlbrew)
;;; perlbrew.el ends here
