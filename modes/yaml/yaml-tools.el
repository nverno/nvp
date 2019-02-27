;;; yaml-tools.el --- yaml things -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/yaml-tools
;; Last modified: <2019-02-23 21:40:05>
;; Package-Requires: 
;; Copyright (C) 2016, Noah Peart, all rights reserved.
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

;; [![Build Status](https://travis-ci.org/nverno/yaml-tools.svg?branch=master)](https://travis-ci.org/nverno/yaml-tools)

;; Better indentation for `yaml-mode'.

;; To use, set `indent-line-function' and `indent-region-function'
;; to be `yaml-indent-indent-line' and `yaml-indent-indent-region'
;; respectively in `yaml-mode' hook, eg

;; ```lisp
;; (defun my-yaml-hook ()
;;   (setq-local indent-line-function 'yaml-indent-indent-line)
;;   (setq-local indent-region-function 'yaml-indent-indent-region))
;; (add-hook 'yaml-mode-hook 'my-yaml-hook)
;; ```

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (require 'subr-x)
  (defvar company-dabbrev-code-modes))
(require 'yaml-indent)
(declare-function nvp-log "nvp-log")
(nvp-declare "" nvp-indicate-modeline-success)

(nvp-package-define-root :snippets t)

(defvar yaml-lint-buffer-name "*travis*")

;; -------------------------------------------------------------------
;;; Validation / Linting 

;;; Ysh

(defun yaml-tools-ysh-install ()
  ())

;;; Travis

;; validate with linter
(defun yaml-tools-travis-lint ()
  (interactive)
  (if-let* ((travis (executable-find "travis")))
      (progn
        (set-process-filter
         (start-process "travis" (nvp-comint-buffer yaml-lint-buffer-name
                                                    (erase-buffer))
                        travis "lint" buffer-file-name)
         #'yaml-tools-travis-sentinel)
        ;; wont return on windows ionno
        (process-send-eof "travis"))
    (nvp-log "Installing travis")
    ;; (nvp-with-process-log 
    ;;   (start-process "gem" (nvp-process-buffer) "gem"
    ;;                  "install" "travis" "--no-rdoc" "--no-ri")
    ;;   :pop-on-error
    ;;   (yaml-tools-travis-lint))
    ))

(defun yaml-tools-travis-sentinel (p m)
  (nvp-log (format "%s: %s" (process-name p) m) yaml-lint-buffer-name)
  (nvp-with-comint-buffer yaml-lint-buffer-name
    (ansi-color-apply-on-region (point-min) (point-max))
    (goto-char (point-min))
    (if (looking-at-p ".*valid")
        (nvp-indicate-modeline-success "iriiiiieeeee")
      (pop-to-buffer yaml-lint-buffer-name))))

;; goto travis page for project, or profile with prefix arg
(defun yaml-tools-travis-project (&optional arg)
  (let* ((name (yaml-tools-project-name))
         (repo (format "https://travis-ci.org/%s"
                       (if arg "profile/nverno"
                         (concat "nverno/" name)))))
    (browse-url repo)))

;;; Appveyor

(defun yaml-tools-appveyor-project (&optional arg)
  (let* ((name (yaml-tools-project-name))
         (repo (format "https://ci.appveyor.com/project%s"
                       (if arg "s" (concat "/nverno/" name)))))
    (browse-url repo)))

;; validate
(defun yaml-tools-appveyor-validate ()
  (interactive)
  (kill-ring-save (point-min) (point-max))
  (browse-url "https://ci.appveyor.com/tools/validate-yaml"))

;; ------------------------------------------------------------
;;; Functions for both

(defun yaml-tools-project-name (&optional arg)
  (let* ((project
          (or arg
              (locate-dominating-file
               (or buffer-file-name default-directory) ".git")))
         (name (if arg
                   (read-from-minibuffer "Project name: ")
                 (if project 
                     (file-name-nondirectory
                      (directory-file-name project))))))
    (or name (yaml-tools-project-name t))))

;; get type of yaml, prompt with prefix
(defun yaml-tools-type (&optional arg)
  (let ((type (if arg
                  (completing-read "Type: " '("travis" "appveyor"))
                buffer-file-name)))
    (cond
     ((string-match-p "travis" type) 'travis)
     ((string-match-p "appveyor" type) 'appveyor)
     (t (yaml-tools-type t)))))

;; execute action depending on type of yaml
(defun yaml-tools-execute (arg actions)
  (let* ((type (yaml-tools-type arg))
         (action (cdr (assq type actions))))
    (pcase action
      ((pred symbolp) (funcall action))
      ((pred listp) (apply (car action) (cdr action)))
      (_ (user-error "Don't know how to handle %S" action)))))

;; validate yaml: for travis use linter, for appveyor use web
(defun yaml-tools-validate (arg)
  (interactive "P")
  (yaml-tools-execute
   arg '((travis   . yaml-tools-travis-lint)
         (appveyor . yaml-tools-appveyor-validate))))

;; yaml description
(defun yaml-tools-info (arg)
  (interactive "P")
  (yaml-tools-execute
   arg
   '((travis . (browse-url "https://docs.travis-ci.com"))
     (appveyor . (browse-url "https://www.appveyor.com/docs/appveyor-yml/")))))

;; open project url, or with prefix arg open profile projects page
(defun yaml-tools-project-info (arg)
  (interactive "P")
  (yaml-tools-execute
   nil `((travis   . (yaml-tools-travis-project ,arg))
         (appveyor . (yaml-tools-appveyor-project ,arg)))))

;; ------------------------------------------------------------
;;; Setup

(defun nvp-yaml-setup-indent ()
  ;; setup indent functions for lines and region
  (setq-local indent-line-function 'yaml-indent-indent-line)
  (setq-local indent-region-function 'yaml-indent-indent-region))

(with-eval-after-load 'company
  (add-to-list 'company-dabbrev-code-modes 'yaml-mode))

(provide 'yaml-tools)
;;; yaml-tools.el ends here
