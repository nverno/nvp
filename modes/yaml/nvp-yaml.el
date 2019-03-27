;;; nvp-yaml.el --- yaml things -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/yaml-tools
;; Last modified: <2019-03-26 21:04:16>
;; Created:  2 November 2016

;;; Commentary:

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
(nvp-declare "" nvp-indicate-modeline)

(nvp-package-define-root :snippets t)

(defvar nvp-yaml-lint-buffer-name "*travis*")

;; -------------------------------------------------------------------
;;; Validation / Linting 

;;; Ysh

(defun nvp-yaml-ysh-install ()
  ())

;;; Travis

;; validate with linter
(defun nvp-yaml-travis-lint ()
  (interactive)
  (if-let* ((travis (executable-find "travis")))
      (progn
        (set-process-filter
         (start-process "travis" (nvp-comint-buffer nvp-yaml-lint-buffer-name
                                                    (erase-buffer))
                        travis "lint" buffer-file-name)
         #'nvp-yaml-travis-sentinel)
        ;; wont return on windows ionno
        (process-send-eof "travis"))
    (message "travis gem not found")))

(defun nvp-yaml-travis-sentinel (p m)
  (nvp-log (format "%s: %s" (process-name p) m) nvp-yaml-lint-buffer-name)
  (nvp-with-comint-buffer nvp-yaml-lint-buffer-name
    (ansi-color-apply-on-region (point-min) (point-max))
    (goto-char (point-min))
    (if (looking-at-p ".*valid")
        (nvp-indicate-modeline "iriiiiieeeee")
      (pop-to-buffer nvp-yaml-lint-buffer-name))))

;; goto travis page for project, or profile with prefix arg
(defun nvp-yaml-travis-project (&optional arg)
  (let* ((name (nvp-yaml-project-name))
         (repo (format "https://travis-ci.org/%s"
                       (if arg "profile/nverno"
                         (concat "nverno/" name)))))
    (browse-url repo)))

;;; Appveyor

(defun nvp-yaml-appveyor-project (&optional arg)
  (let* ((name (nvp-yaml-project-name))
         (repo (format "https://ci.appveyor.com/project%s"
                       (if arg "s" (concat "/nverno/" name)))))
    (browse-url repo)))

;; validate
(defun nvp-yaml-appveyor-validate ()
  (interactive)
  (kill-ring-save (point-min) (point-max))
  (browse-url "https://ci.appveyor.com/tools/validate-yaml"))

;; ------------------------------------------------------------
;;; Functions for both

(defun nvp-yaml-project-name (&optional arg)
  (let* ((project
          (or arg
              (locate-dominating-file
               (or buffer-file-name default-directory) ".git")))
         (name (if arg
                   (read-from-minibuffer "Project name: ")
                 (if project 
                     (file-name-nondirectory
                      (directory-file-name project))))))
    (or name (nvp-yaml-project-name t))))

;; get type of yaml, prompt with prefix
(defun nvp-yaml-type (&optional arg)
  (let ((type (if arg
                  (completing-read "Type: " '("travis" "appveyor"))
                buffer-file-name)))
    (cond
     ((string-match-p "travis" type) 'travis)
     ((string-match-p "appveyor" type) 'appveyor)
     (t (nvp-yaml-type t)))))

;; execute action depending on type of yaml
(defun nvp-yaml-execute (arg actions)
  (let* ((type (nvp-yaml-type arg))
         (action (cdr (assq type actions))))
    (pcase action
      ((pred symbolp) (funcall action))
      ((pred listp) (apply (car action) (cdr action)))
      (_ (user-error "Don't know how to handle %S" action)))))

;; validate yaml: for travis use linter, for appveyor use web
(defun nvp-yaml-validate (arg)
  (interactive "P")
  (nvp-yaml-execute
   arg '((travis   . nvp-yaml-travis-lint)
         (appveyor . nvp-yaml-appveyor-validate))))

;; yaml description
(defun nvp-yaml-info (arg)
  (interactive "P")
  (nvp-yaml-execute
   arg
   '((travis . (browse-url "https://docs.travis-ci.com"))
     (appveyor . (browse-url "https://www.appveyor.com/docs/appveyor-yml/")))))

;; open project url, or with prefix arg open profile projects page
(defun nvp-yaml-project-info (arg)
  (interactive "P")
  (nvp-yaml-execute
   nil `((travis   . (nvp-yaml-travis-project ,arg))
         (appveyor . (nvp-yaml-appveyor-project ,arg)))))

;; ------------------------------------------------------------
;;; Setup

(defun nvp-yaml-setup-indent ()
  ;; setup indent functions for lines and region
  (setq-local indent-line-function 'yaml-indent-indent-line)
  (setq-local indent-region-function 'yaml-indent-indent-region))

(with-eval-after-load 'company
  (add-to-list 'company-dabbrev-code-modes 'yaml-mode))

(provide 'nvp-yaml)
;;; nvp-yaml.el ends here
