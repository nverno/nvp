;;; nvp-ruby-install.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/ruby-tools
;; Last modified: <2019-03-08 06:25:24>
;; Package-Requires: 
;; Created:  6 December 2016

;;; Commentary:
;; FIXME: remove most of this
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'nvp-ruby)

(autoload 'nvp-env-setenv! "nvp-env")
(autoload 'nvp-env-path-add "nvp-env")
(autoload 'asdf--versions "asdf")
(autoload 'asdf-install "asdf")
(autoload 'nvp-log "nvp-log")
(autoload 'nvp-ext-run-script "nvp-ext")

;; ------------------------------------------------------------
;;; Install / Environment

;; (nvp-with-gnu/w32
;;     ;; Install ruby (default to latest version), default gems, completion, and
;;     ;; version manager.  With prefix, prompt for ruby version and configure options.
;;     (defun nvp-ruby-install (arg)
;;       (interactive "P")
;;       (nvp-with-asdf-install
;;        arg (nvp-package-root) "ruby" "--with-tcl --with-tk" nil
;;        (lambda () (nvp-ruby-install nil))))

;;   (defun nvp-ruby-install ()
;;     (interactive)
;;     (user-error "Not implemented")))

;; (defun nvp-ruby-install-rvm ()
;;   (interactive)
;;   (set-process-sentinel 
;;    (nvp-with-process-filter
;;      ;; gpg keys
;;      (nvp-ext-run-script
;;       (expand-file-name "script/install.sh" (nvp-package-root))
;;       '("install_rvm_deps") 'sudo))
;;    (nvp-with-sentinel nil
;;      (nvp-with-process-log
;;        ;; install rvm / ruby
;;        (nvp-ext-run-script
;;         (expand-file-name "script/install.sh" (nvp-package-root))
;;         '("install_rvm"))))))

;; set ruby environment variables
(defun nvp-ruby-setenv! ()
  (let ((gemp (expand-file-name
               (concat ".gem/ruby/" (nvp-ruby-version))
               (getenv "HOME"))))
    ;; Add gem path to user PATH if not already there, and
    ;; update exec-path in current session
    (nvp-env-setenv! "PATH" (expand-file-name "bin" gemp) 'exec)
    ;; Add GEM_HOME to enviornment, overwrite it if it is already
    ;; there since it changes with ruby version.
    (nvp-env-setenv! "GEM_HOME" gemp 'exec 'clobber)))

;; add gems to exec path, but don't change environment
;;;###autoload
(defun nvp-ruby-add-gems-path ()
  (interactive)
  (nvp-env-path-add
   (directory-file-name
    (expand-file-name (concat ".gem/ruby/" (nvp-ruby-version))
                      (getenv "HOME")))))

;;; Gems

;; uninstall gems
(defun nvp-ruby--uninstall-gems ()
  (start-process "ruby" "*nvp-install*" "ruby" "-e"
                 (nvp-concat "`gem list`.split(/$/).each {|line| "
                             "puts `gem uninstall -Iax #{line.split(' ')[0]}`"
                             " unless line.empty? }")))

;; rake, bundler, pry, pry-doc, method-source
(cl-defun nvp-ruby--install-gems
    (&key
     ;; if non-nil, update gem index first
     (update t)
     ;; list of gems to install
     (gems '("rake" "bundler" "pry" "pry-doc" "method_source")))
  ;; need lexical binding here for process sentinels
  (let ((mods gems))
    (cl-flet ((install-gems ()
                (nvp-with-process "gem"
                    :proc-args ("install" "-q" mods))))
      (if update
          (nvp-with-process "gem"
            :proc-args ("update" "--system")
            :on-success (install-gems))
        ;; just install gems
        (install-gems)))))

;;;###autoload
(defun nvp-ruby-install-gems (gems)
  "Install gems, update first with prefix."
  (interactive
   (list (read-from-minibuffer "Install Gems (blank for default): ")))
  (when gems
    (if (string= "" gems)
        (nvp-ruby--install-gems :update current-prefix-arg)
      (nvp-ruby--install-gems :update current-prefix-arg
                               :gems (split-string gems)))))

;; interactive interface to install/uninstall/update
;;;###autoload
(defun nvp-ruby-install-ruby (uninstall env update gems)
  (interactive
   (if (y-or-n-p "Uninstall?")
       (list t nil nil nil)
     (list
      nil
      (nvp-with-w32 (y-or-n-p "Set env?"))
      (y-or-n-p "Update gem index?")
      (if (y-or-n-p "Install default gems?")
          'default
        (split-string (read-from-minibuffer "Gems: "))))))
  (nvp-ruby-install-setup :uninstall uninstall
                           :env env
                           :update update
                           :gems gems))

(cl-defun nvp-ruby-install-setup (&key
                                   (uninstall nil)
                                   (env t)
                                   (update t)
                                   (gems 'default))
  ;; FIXME: do I need to bind here for lexical??
  (let ((u uninstall)
        (e env)
        (g gems)
        (up update))
    (cond
     ;; uninstall first
     (u
      (set-process-sentinel
       (nvp-ruby--uninstall-gems)
       #'(lambda (p _m)
           (when (zerop (process-exit-status p))
             (nvp-ruby-install-setup :uninstall nil
                                      :env e
                                      :gems g)))))
     ;; update environment variables
     (e
      (nvp-ruby-setenv!)
      (nvp-ruby-install-setup :uninstall nil
                               :env nil
                               :gems g))
     ;; install gems
     (g
      (if (eq 'default g)
          (nvp-ruby--install-gems :update up)
        (nvp-ruby--install-gems :update up
                                 :gems g)))
     (t nil))))

(provide 'nvp-ruby-install)
;;; nvp-ruby-install.el ends here
