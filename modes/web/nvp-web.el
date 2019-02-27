;;; nvp-web.el --- web helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/web-tools
;; Last modified: <2019-01-31 01:10:13>
;; Package-Requires: 
;; Created: 31 December 2016

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

;; [![Build Status](https://travis-ci.org/nverno/web-tools.svg?branch=master)](https://travis-ci.org/nverno/web-tools)

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (require 'subr-x)
  (defvar httpd-root)
  (defvar httpd-port))

(autoload 'nvp-jinja-url-for "nvp-jinja")

(nvp-package-define-root :snippets t)

;; -------------------------------------------------------------------
;;; Utils

(defmacro web-tools-css-common-bindings (&rest bindings)
  (declare (indent defun))
  `(progn
     (nvp-bindings "css-mode" nil ,@bindings)
     (nvp-bindings "less-css-mode" nil ,@bindings)
     (nvp-bindings "scss-mode" nil ,@bindings)
     (nvp-bindings "sass-mode" nil ,@bindings)))

;; -------------------------------------------------------------------
;;; Interactive

(nvp-newline web-tools-css-newline nil :pairs (("{" "}")))

;;; Browsing
(declare-function impatient-mode "impatient-mode")
(declare-function imp-visit-buffer "impatient-mode")
(declare-function httpd-start "simple-httpd")

(defun web-tools-browse-buffer()
  (interactive)
  (impatient-mode)
  (httpd-start)
  (imp-visit-buffer))

;; https://github.com/cjohansen/.emacs.d/blob/master/defuns/misc-defuns.el
;; Start httpd-server in current directory.
(defun web-tools-httpd-start-here (directory port)
  (interactive
   (list (read-directory-name "Root directory: " default-directory nil t)
         (read-number "Port: " 8017)))
  (setq httpd-root directory)
  (setq httpd-port port)
  (httpd-start)
  (browse-url (concat "http://localhost:" (number-to-string port) "/")))

;;; Navigation
(declare-function web-mode-element-beginning "web-mode")
(declare-function web-mode-element-previous "web-mode")
(declare-function web-mode-element-next "web-mode")
(declare-function web-mode-element-end "web-mode")

(defun web-tools-backward-tag ()
  (interactive)
  (and (eq (char-before) ?>)
       (backward-char))
  (if (eq (char-after) ?<)
      (web-mode-element-previous)
    (web-mode-element-beginning)))

(defun web-tools-forward-tag ()
  (interactive)
  (if (eq (char-before) ?>)
      (web-mode-element-next)
    (web-mode-element-end)))

;;; Jump
(autoload 'projectile-project-root "projectile")
(declare-function web-mode-tag-match "web-mode")
(declare-function web-mode-block-beginning "web-mode")
(declare-function xref-push-marker-stack "xref")

(defun web-tools-href-at-point ()
  (save-excursion
    (pcase (bound-and-true-p engine)
      (`jinja2 (nvp-jinja-url-for))
      (`django (nvp-jinja-url-for))
      (_ (if (not (member (get-text-property (point) 'tag-name) '("a" "link")))
             (user-error "Can't find link at point")
           (if (eq (get-text-property (point) 'tag-type) 'end)
               (web-mode-tag-match)
             (web-mode-element-beginning))
           (and (re-search-forward "href=\"\\(\[^\"\]+\\)" (line-end-position) 'move)
                (match-string 1)))))))

(defun web-tools-find-href (&optional href)
  (interactive (list (web-tools-href-at-point)))
  (when href
    (xref-push-marker-stack)
    (find-file (expand-file-name href (projectile-project-root)))))

;;; Compile
(defun web-tools-scss-compile ()
  (interactive)
  (let* ((compile-command
          (concat "sass " (buffer-file-name) " "
                  (concat (file-name-sans-extension (buffer-file-name)) ".css"))))
    (call-interactively 'nvp-compile)))

(eval-when-compile
  (defvar web-mode-engine))
(defun web-tools-help-at-point ()
  (interactive)
  (cond
   ((cl-member web-mode-engine '("django" "jinja2") :test 'string=)
    (browse-url "http://jinja.pocoo.org/docs/2.10/"))
   (t (message "TODO"))))

(provide 'nvp-web)
;;; nvp-web.el ends here
