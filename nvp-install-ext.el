;;; nvp-installer-ext.el --- Install external deps -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-24 17:53:37>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 14 February 2019

;;; Commentary:

;; Interface to install external dependencies
;; TODO:
;; - sentinels for external installs
;; - better logging
;; - `make-progress-reporter'
;; - eventually merge back into nvp-install
;; - gather targets: eg. make -prRn

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'nvp-macro)
  (nvp-local-vars))
(require 'make-mode)
(require 'nvp)

(cl-defstruct (nvp-mode (:constructor nvp-mode--make)
                        (:copier nil))
  "Mode configuration and dependencies."
  depends                               ;modes on which it depends
  pkgs                                  ;pkgs using default manager
  external                              ;external dependencies
  paths                                 ;load-paths
  )

(cl-defmethod nvp-install-help (&optional _type)
  "Display help about mode, eg. dependencies, packages, paths, etc."
  (nvp-with-results-buffer (help-buffer)
    (call-process "make" nil t t (concat "--file=" nvp/makeext) "help")))

;; -------------------------------------------------------------------
;;; Manage external dependencies

(cl-defstruct (nvp-install-ext (:constructor nvp-install-ext--make)
                               (:copier nil))
  "Struct to hold external installer info."
  (mode :read-only) (makefile :read-only) (targets :read-only))

(cl-defun nvp-install-ext-make (targets &key mode (makefile nvp/makeext))
  (nvp-install-ext--make :targets targets :mode mode :makefile makefile))

(defun nvp-install-help (ext)
  (nvp-with-results-buffer (help-buffer)
    (call-process-shell-command
     (concat "make -C " (file-name-directory (nvp-install-ext-makefile ext)) " help")
     nil t t)))

;; (setq tst (nvp-install-ext-make '("default") :mode "elisp"))
;; (defun nvp-installer--call (installer &rest args)
;;   )

;; (defun nvp-installer--help (&optional makefile)
;;   "Return MAKEFILE help doc."
;;   (with-output-to-temp-buffer)
;;   )

(defun nvp-installer--make-targets (&optional makefile)
  "List available targets in MAKEFILE, defaulting to `nvp-installer-makefile'."
  (with-temp-buffer
    (insert-file-contents (or makefile nvp/makeext))
    (makefile-pickup-targets)
    makefile-target-table))

;; (defun nvp-installer-make-target (&optional makefile &rest targets)
;;   "Install MAKEFILE TARGETS."
;;   (interactive (list nvp-installer-makefile)))


(provide 'nvp-installer-ext)
;;; nvp-installer-ext.el ends here
