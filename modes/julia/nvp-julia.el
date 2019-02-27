;;; nvp-julia.el --- julia mode utilities -*- lexical-binding: t -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp-julia
;; Last modified: <2019-02-21 00:16:18>
;; Package-Requires: 
;; Copyright (C) 2016, Noah Peart, all rights reserved.
;; Created: 28 September 2016

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

;; TODO:
;; - add company jump to location function

;;; Code:

(eval-when-compile
  (require 'nvp-macro)
  (require 'subr-x)
  (require 'cl-lib))
(require 'company)
(require 'julia-mode)
(require 'ess-site)
(require 'ess-julia)
(declare-function macrostep-expand "macrostep")
(declare-function pos-tip-show "pos-tip")

(autoload 'tag-utils-tag-dir "tag-utils")
(autoload 'nvp-log "nvp-log")
(autoload 'nvp-ext-sudo-command "nvp-ext")
(autoload 'nvp-r-help-at-point "nvp-r")

;; -------------------------------------------------------------------
;;; Vars 

(nvp-package-define-root :snippets t)

(defvar nvp-julia-source-repo "https://github.com/JuliaLang/julia")
(defvar nvp-julia-source-dir
  (expand-file-name "julia" (or (bound-and-true-p tag-utils-source-dir)
                                (getenv "DEVEL"))))

;; ------------------------------------------------------------
;;; Tags

(defun nvp-julia-tag-source (&optional noretry)
  "Tag julia source code. Prefix to force retag."
  (interactive)
  (let* ((tags (expand-file-name "TAGS" nvp-julia-source-dir))
         (no-src (not (file-exists-p nvp-julia-source-dir)))
         (no-tags (not (file-exists-p tags))))
    (unless noretry
      (cond
       (no-src
        (nvp-log "Cloning julia repo: %s" nil nvp-julia-source-repo)
        (nvp-julia-tag-sentinel
         (start-process "jl-tags" "*jl-tags*" "git" "clone"
                        nvp-julia-source-repo
                        nvp-julia-source-dir)
         nil))
       ((or current-prefix-arg no-tags)
        (nvp-log "Creating julia source TAGS")
        ;; nvp-julia-tag-sentinel
        (tag-utils-tag-dir nvp-julia-source-dir
                           :program
                           (or (bound-and-true-p tag-utils-ctags-exe)
                               "ctags")))
       (t (visit-tags-table tags))))))

(defun nvp-julia-tag-sentinel (proc &optional noretry)
  (set-process-sentinel
   proc #'(lambda (p m)
            (nvp-log "%s: %s" nil (process-name p) m)
            (when (eq 0 (process-exit-status p))
              (nvp-julia-tag-source noretry)))))

;; -------------------------------------------------------------------
;;; Help 

(defun nvp-julia-help-at-point ()
  (interactive)
  (if (functionp 'nvp-r-help-at-point)
      (call-interactively 'nvp-r-help-at-point)
    ;; FIXME
    (when-let* ((sym (thing-at-point 'symbol)))
      (with-current-buffer 
          (company-doc-buffer
           (ess-julia-get-object-help-string sym))
        (buffer-substring-no-properties (point-min) (point-max))))))

;; toggle pophelp help for symbol at point in pos-tip
;; (defun nvp-julia-popup-help-at-point ()
;;   (interactive)
;;   (nvp-with-toggled-tip (nvp-julia-popup-doc-at-point)))

;; -------------------------------------------------------------------
;;; Yas
(declare-function yas-text "yasnippet")

;; Split a julia argument string into ((name, default)..) tuples
(defun nvp-julia-split-args (arg-string)
  (and arg-string
       (mapcar (lambda (x)
                 (split-string x "[[:blank:]]*=[[:blank:]]*" t))
               (split-string arg-string "[[:blank:]]*[,;][[:blank:]]*" t))))
 
;; return docstring format for the julia arguments in yas-text
(defun nvp-julia-args-to-docstring ()
  (if-let* ((indent (concat "\n" (make-string (current-column) ? )))
            (args (nvp-julia-split-args (yas-text)))
            (max-len
             (if args
                 (apply 'max (mapcar (lambda (x) (length (nth 0 x))) args))
               0))
            (formatted-args
             (mapconcat
              (lambda (x)
                (concat "- "
                        (nth 0 x)
                        (make-string (- max-len (length (nth 0 x))) ? )
                        " : "
                        (if (nth 1 x)
                            (concat "\(default " (nth 1 x) "\)"))))
              args
              indent)))
      (if (string-empty-p formatted-args) ""
        (mapconcat 'identity (list "# Arguments" formatted-args) indent))
    ""))

(provide 'nvp-julia)
;;; nvp-julia.el ends here
