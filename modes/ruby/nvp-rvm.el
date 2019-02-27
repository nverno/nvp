;;; nvp-rvm.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/ruby-tools
;; Last modified: <2019-01-25 23:50:16>
;; Package-Requires: 
;; Created:  6 December 2016

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

;; TODO: unused
;; - install gems with rvm
;;
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (defvar eshell-path-env))
(require 'nvp-ruby)
(autoload 'nvp-log "nvp-log")

(defvar nvp-rvm-verbose t)
(defvar nvp-rvm-exe (executable-find "rvm"))
(defvar nvp-rvm-buffer "*ruby-rvm*")

(defvar nvp-rvm--ruby nil)
(defvar nvp-rvm--gemset nil)
(eval-and-compile (defvar nvp-rvm--gemset-sep "@"))
(defvar nvp-rvm--gemset-default "global")

;; regular expression to parse the .rvmrc files inside project
;; directories. the first group matches the ruby-version and the
;; second group is the gemset. when no gemset is set, the second group
;; is nil
(defvar nvp-rvm--rvmrc-parse-regexp
  (nvp-concat
   "\\(?:^rvm\s+\\(?:use\s+\\|\\)\\|environment_id=\"\\)\s*"
   "\\(?:--.+\s\\)*" ;; Flags
   "\\([^"
   nvp-rvm--gemset-sep
   "\n]+\\)\\(?:"
   nvp-rvm--gemset-sep
   "\\([^\"\s\n]+\\)\\)?\\(?:\"\\|\\)"))

;; -------------------------------------------------------------------
;;; Util

;; do BODY in process buffer of CALL
(defmacro with-rvm-buffer (call &rest body)
  (declare (indent defun) (debug t))
  `(with-current-buffer (get-buffer-create nvp-rvm-buffer)
     (erase-buffer)
     (let ((stat ,call))
       (if (not (zerop stat))
           (nvp-log (buffer-string))
         ,@body))))

;; call rvm with ARGS, output at point
(defmacro nvp-rvm (&rest args)
  `(apply 'call-process
          nvp-rvm-exe nil t nil (delq nil (list ,@args))))

(defmacro cassoc (key data)
  `(cdr (assoc ,key ,data)))

;; available rvm help docs
(defsubst nvp-rvm--help-topics ()
  (let ((dir (getenv "rvm_path")))
    (when dir
      (mapcar
       #'(lambda (s) (substring s 0 -3))
       (directory-files (expand-file-name "help" dir) nil "\.md$")))))

;; name for RUBY version and GEMSET
(defsubst nvp-rvm--name (ruby gemset)
  (if (string= gemset nvp-rvm--gemset-default)
      ruby
    (concat gemset nvp-rvm--gemset-sep gemset)))

;; Return path with all current ruby rvm entries removed.
;; Entries are removed if they are prefixed with 'rvm_path'
;; and they string-match against VERSION
(defsubst nvp-rvm--filter-path (version &optional rvm-path)
  (when version 
    (let ((rvmp (or rvm-path (getenv "rvm_path")))
          (ver (regexp-quote version)))
      (unless rvmp
        (user-error "rmv_path not found."))
      (cl-remove-if
       #'(lambda (p)
           (and (string-prefix-p rvmp p)
                (string-match-p ver p)))
       (split-string (getenv "PATH") path-separator)))))

;; Update environment variables and exec-path for new ruby.
;; All environment variables from rvm are synced, except PATH is
;; treated specially since it differ from shell path.  For PATH,
;; entries are filtered by `nvp-rvm--filter-path', and new
;; entries are added from 'rvm info %s'.
;; `nvp-rvm--ruby'
(defsubst nvp-rvm--sync-env (ruby)
  (let* ((path (nvp-rvm--filter-path nvp-rvm--ruby))
         (info (nvp-rvm-info ruby))
         (env (car info))
         (gemp (cassoc "GEM_PATH" env))
         (rubyp (file-name-directory (cassoc "ruby" (cdr info)))))
    (setq path (nconc
                `(,@(mapcar (lambda (p) (expand-file-name "bin" p))
                            (split-string gemp path-separator))
                  ,rubyp)
                path))
    (setq exec-path path)
    (setenv "PATH" (mapconcat 'identity path path-separator))
    (dolist (env-var env)
      (unless (string= (car env-var) "PATH")
        (setenv (car env-var) (cdr env-var))))
    (setenv "BUNDLE_PATH" (or (cassoc "GEM_HOME" env) ""))
    (setq eshell-path-env (getenv "PATH"))))

;; -------------------------------------------------------------------
;;; Interface to rvm

;; get list of available rubies: 'rvm list [default]'
;; and sets `nvp-rvm--ruby'
(defun nvp-rvm-list (&optional default)
  (with-rvm-buffer
    (nvp-rvm "list" (and default "default"))
    (goto-char (point-min))
    (let ((re (nvp-concat
               "^[ \t]*\\(=?[>*]\\)?[ \t]*"  ;; current version?
               "\\(.+?\\)[ \t]*"             ;; ruby version
               "\\[ *\\(.+\\) *\\][ \t]*$")) ;; arch
          ruby rubies)
      (while (not (eobp))
        (when (looking-at re)
          (setq ruby (match-string 2))
          (and ruby (push ruby rubies))
          (and (match-string 1) (setq nvp-rvm--ruby ruby)))
        (forward-line 1))
      rubies)))

;; get list of gemsets for this RUBY
(defun nvp-rvm-gemset-list (ruby)
  (with-rvm-buffer
    (nvp-rvm "gemset" "list_all")
    (goto-char (point-min))
    (let ((re (format "^\\(?:[Gg]emset\\)[^\n]+\\(?:%s\\)" ruby))
          gemsets)
      (while (not (eobp))
        (when (looking-at-p re)
          (forward-line)
          (while (not (or (eobp) (looking-at-p "^[Gg]emset")))
            (and (looking-at "[ \t=>(]*\\([^ )\t\n\r]+\\)")
                 (push (match-string 1) gemsets))
            (forward-line)))
        (forward-line))
      gemsets)))

;; return assoc list of 'rvm info %s' data of form:
;; ((environment variables) (other))
(defun nvp-rvm-info (&optional ruby)
  (with-rvm-buffer
    (nvp-rvm "info" ruby)
    (goto-char (point-min))
    (let ((re "\s+\\(.+?\\):\s+\"\\(.+?\\)\"")
          res env)
      (while (not (eobp))
        (when (looking-at-p "^[ \t]*\\(?:environment:\\)")
          ;; store environment key separately
          (forward-line)
          (while (looking-at re)
            (push (cons (match-string 1) (match-string 2)) env)
            (forward-line)))
        ;; rest of keys
        (and (looking-at re)
             (push (cons (match-string 1) (match-string 2)) res))
        (forward-line))
      (cons env res))))

;;;###autoload
(defun nvp-rvm-use (ruby gemset)
  "Set current ruby/gemset with rvm."
  (interactive
   (let* ((ruby (ido-completing-read
                 "Ruby version: " (nvp-rvm-list)))
          (gemset (ido-completing-read
                   "Gemset: " (nvp-rvm-gemset-list ruby))))
     (list ruby gemset)))
  (let ((name (nvp-rvm--name ruby gemset)))
    (nvp-rvm--sync-env name)
    (setq nvp-rvm--ruby ruby
          nvp-rvm--gemset gemset)
    (when nvp-rvm-verbose
      (message "%s activated" name))))

;; -------------------------------------------------------------------
;;; Read .rvmrc files

(defsubst nvp-rvm-locate-rvmrc ()
  (condition-case nil
      (expand-file-name
       ".rvmrc" (locate-dominating-file buffer-file-name ".rvmrc"))
    (error nil)))

(defsubst nvp-rvm--rvmrc-version ())
;; -------------------------------------------------------------------
;;; Help

;;;###autoload
(defun nvp-rvm-cheatsheet ()
  (interactive)
  (browse-url "http://cheat.errtheblog.com/s/rvm"))

;;;###autoload
(defun nvp-rvm-help-doc ()
  (interactive)
  (ido-find-file-in-dir
   (expand-file-name "help" (getenv "rvm_path"))))

(provide 'ruby-rvm)
;;; nvp-rvm.el ends here
