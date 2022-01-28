;;; nvp-yaml.el --- yaml things -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Better indentation for `yaml-mode'.
;;
;; To use, set `indent-line-function' and `indent-region-function'
;; to be `yaml-indent-indent-line' and `yaml-indent-indent-region'
;; respectively in `yaml-mode' hook, eg
;;
;; ```lisp
;; (defun my-yaml-hook ()
;;   (setq-local indent-line-function 'yaml-indent-indent-line)
;;   (setq-local indent-region-function 'yaml-indent-indent-region))
;; (add-hook 'yaml-mode-hook 'my-yaml-hook)
;; ```
;;
;; CI configs
;; ~~~~~~~~~~
;; Currently recognized: travis, appveyor, circleci
;;
;; Linting support:
;; - travis (gem install travis)
;; - circleci (build/circleci)
;;
;; XXX: possibly look into YSH?
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'nvp-yaml-indent)
(nvp:decls)

;; recognized CI types -- for jumping to info/linting
;; forms: (type prompt-key prompt-message linter &rest linter-args)
(eval-and-compile
  (defvar nvp-yaml-ci-types
    `((travis
       (prompt ?t "[t]ravis")
       (validate nvp-yaml-validate-call "travis" "lint" (buffer-file-name))
       (help "https://docs.travis-ci.com")
       ;; project info format:
       ;; 1. base uri
       ;; 2. display project repo (if function call it to produce format args,
       ;;    otherwise treat as string)
       ;; 3. with prefix, display page with all repos
       (project "https://travis-ci.org/%s" nvp-yaml-project-repo "profile/nverno"))
      (circleci
       (prompt ?c "[c]irclci")
       (validate nvp-yaml-validate-call "circleci" "config" "validate"
                 (buffer-file-name))
       (help format "https://circleci.com/docs/2.0/configuration-reference/#%s")
       (project "https://circleci.com/%s"
                (lambda () (concat "gh/" (nvp-yaml-project-repo))) "dashboard"))
      (appveyor
       (prompt ?a "[a]ppveyor")
       (validate nvp-yaml-validate-appveyor)
       (help "https://www.appveyor.com/docs/appveyor-yml/")
       (project "https://ci.appveyor.com/project%s" nvp-yaml-project-repo "s"))
      (codecov
       (prompt ?v "codeco[v]")
       (validate nvp-yaml-validate-call "curl" "--data-binary"
                 (concat "@" (file-name-nondirectory (buffer-file-name)))
                 "https://codecov.io/validate")
       (help "https://docs.codecov.io/docs/codecovyml-reference")
       (project "https://codecov.io/gh/%s" nvp-yaml-project-repo "")))))

(eval-when-compile
  (defsubst nvp-yaml--value (type val)
    (cdr (assq val (assq type nvp-yaml-ci-types)))))

;; prompt for yaml type from list of known types
(defun nvp-yaml-read-known-type ()
  (eval-when-compile
    (macroexpand-all
     `(nvp:read-char-case "Yaml type: " 'verbose
        ,@(cl-loop for type in nvp-yaml-ci-types
             for prompt = (nvp-yaml--value (car type) 'prompt)
             collect (list (car prompt) (cadr prompt) `(quote ,(car type))))))))

;; full path to git repo
(defun nvp-yaml-project-url ()
    (condition-case nil
        (car (process-lines "git" "config" "--get" "remote.origin.url"))
      (error nil)))

;; return the git repo name of current project
(defun nvp-yaml-project-repo ()
  (--when-let (nvp-yaml-project-url)
    (replace-regexp-in-string "https://github.com/" "" it nil 'literal)))

;; either guess project name based on project root's directory name, or
;; prompt with prefix ARG
(defun nvp-yaml-project-name (&optional arg)
  (if arg (read-from-minibuffer "Project name: ")
    (--when-let (or (nvp-yaml-project-url)
                    (nvp-project-root))
      (file-name-nondirectory (directory-file-name it)))))

;; guess or prompt for type of yaml config (eg. CI)
(defun nvp-yaml-type (&optional prompt)
  (if prompt (nvp-yaml-read-known-type)
    (let ((dir (nvp:path 'ds (buffer-file-name)))
          (fname (nvp:bfn)))
      (cond
       ((string-prefix-p ".appveyor" fname) 'appveyor)
       ((string-prefix-p "codecov." fname) 'codecov)
       ((string= dir ".circleci") 'circleci)
       ((or (string-prefix-p ".travis" fname)
            (string= dir "travis"))
        'travis)
       ;; auto-guessing failed -- so just prompt
       (t (nvp-yaml-type 'prompt))))))

;; execute ACTION depending on type of yaml -- prompt with ARG
(defun nvp-yaml-execute (action &optional arg &rest args)
  (-some--> (nvp-yaml-type arg)
    (nvp-yaml--value it action)
    (pcase it
      ((pred stringp) (car it))
      ((or (pred functionp) (pred symbolp)) (funcall it))
      ((pred listp)
       (if (stringp (car it)) it (apply (car it) (append (cdr it) args))))
      (_ (user-error "Don't know how to handle %S" it)))))

;; -------------------------------------------------------------------
;;; Validating yaml

(defun nvp-yaml-validate (&optional arg)
  "Validate yaml buffer. Prompt for type with prefix ARG."
  (interactive "P")
  (nvp-yaml-execute 'validate arg))

(defun nvp-yaml-validate-appveyor ()
  "Validate buffer contents online for appveyor."
  (interactive)
  (kill-ring-save (point-min) (point-max))
  (browse-url "https://ci.appveyor.com/tools/validate-yaml"))

(defun nvp-yaml-validate-call (type &rest args)
  "Validate buffer using local validator (async)."
  (let* ((prog (nvp:as-string type))
         (proc
          (nvp:with-process prog
            :proc-name prog
            :proc-args (args)
            :proc-buff (nvp:comint-buffer
                         :name (concat "*lint-" prog "*")
                         (erase-buffer))
            :proc-sentinel #'nvp-yaml-lint-sentinel)))
    ;; travis process wont return on windows ionno
    (nvp:with-w32 (ignore-errors (process-send-eof prog)))
    proc))

(defun nvp-yaml-lint-sentinel (p m)
  (with-current-buffer (process-buffer p)
    (if (or (not (zerop (process-exit-status p)))
            (re-search-forward (regexp-opt '("error" "warning")) nil t))
        (progn
          (nvp-indicate-modeline (format "bad yaml: %s" m) 'failure)
          (ansi-color-apply-on-region (point-min) (point-max))
          (goto-char (point-min))
          (pop-to-buffer (process-buffer p)))
      (nvp-indicate-modeline "yaml iriiiiieeeee" 'success)
      (kill-buffer (current-buffer)))))

;; -------------------------------------------------------------------
;;; Help

(defun nvp-yaml-project-info (arg)
  "Open project url or profile page with prefix ARG."
  (interactive "P")
  (--when-let (nvp-yaml-execute 'project)
    (let* ((base (pop it))
           (args (if (null arg)
                     (pcase (car it)
                       ((or (pred symbolp) (pred functionp)) (funcall (car it)))
                       (_ (car it)))
                   (cadr it))))
      (browse-url (format base args)))))

(defun nvp-yaml-help-at-point (sym &optional arg)
  "Attempt to jump to help for SYM at point, or prompt.
Otherwise, goto help reference if available."
  (interactive (list (nvp:tap 'tapi)))
  (--when-let (nvp-yaml-execute 'help arg sym)
    (browse-url it)))

(provide 'nvp-yaml)
;;; nvp-yaml.el ends here
