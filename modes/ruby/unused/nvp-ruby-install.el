;;; nvp-ruby-install.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;; FIXME: gem paths are wrong with asdf
;; FIXME: remove all this (8/31/20)
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-ruby)
(nvp:decls)
(nvp:auto "nvp-env" 'nvp-env-path-add 'nvp-env-setenv!)

(defsubst nvp-ruby-version ()
  (let ((str (shell-command-to-string "ruby --version")))
    (and (string-match "\\([0-9.]+\\)" str)
         (match-string-no-properties 1 str))))

;; ------------------------------------------------------------
;;; Install / Environment

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

(defun nvp-ruby--uninstall-gems ()
  (start-process "ruby" "*nvp-install*" "ruby" "-e"
                 (nvp:concat "`gem list`.split(/$/).each {|line| "
                             "puts `gem uninstall -Iax #{line.split(' ')[0]}`"
                             " unless line.empty? }")))

(cl-defun nvp-ruby--install-gems
    (&key
     (update t)                         ; if non-nil, update gem index first
     (gems '("rake" "bundler" "pry" "pry-doc" "method_source")))
  ;; need lexical binding here for process sentinels
  (let ((mods gems))
    (cl-flet ((install-gems
               ()
               (nvp:with-process "gem"
                 :proc-args ("install" "-q" mods))))
      (if update
          (nvp:with-process "gem"
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

(provide 'nvp-ruby-install)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-ruby-install.el ends here
