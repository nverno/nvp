;;; nvp-yaml.el --- yaml things -*- lexical-binding: t; -*-

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
;;
;; XXX: possibly look into YSH?

;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-yaml-indent)
(nvp-decls)

(defvar nvp-yaml-lint-buffer-name "*travis*")

;; -------------------------------------------------------------------
;;; Utils

(defun nvp-yaml-project-name (&optional arg)
  (let* ((project
          (or arg
              (locate-dominating-file
               (or buffer-file-name default-directory) ".git")))
         (name (if arg (read-from-minibuffer "Project name: ")
                 (if project (nvp-path 'ds project)))))
    (or name (nvp-yaml-project-name t))))

;; get type of yaml, prompt with prefix
(defun nvp-yaml-type (&optional arg)
  (let ((type (if arg (completing-read "Type: " '("travis" "appveyor"))
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

;; -------------------------------------------------------------------
;;; Validating yaml

(defun nvp-yaml-travis-lint ()
  "Validate buffer with travis gem."
  (interactive)
  (let ((travis (executable-find "travis")))
    (if (not travis)
        (user-error "travis gem not found on `exec-path'")
      (nvp-with-process travis
        :proc-name "travis"
        :proc-args ("lint" buffer-file-name)
        :proc-buff (nvp-comint-buffer :name nvp-yaml-lint-buffer-name
                      (erase-buffer))
        :proc-sentinel #'nvp-yaml-travis-sentinel)
      ;; wont return on windows ionno
      (process-send-eof "travis"))))

(defun nvp-yaml-travis-sentinel (p m)
  (nvp-log (format "%s: %s" (process-name p) m) nvp-yaml-lint-buffer-name)
  (with-current-buffer nvp-yaml-lint-buffer-name
    (ansi-color-apply-on-region (point-min) (point-max))
    (goto-char (point-min))
    (if (looking-at-p ".*valid")
        (nvp-indicate-modeline "iriiiiieeeee" 'success)
      (nvp-indicate-modeline "bad things" 'failure)
      (pop-to-buffer nvp-yaml-lint-buffer-name))))

(defun nvp-yaml-appveyor-validate ()
  "Validate buffer contents online for appveyor."
  (interactive)
  (kill-ring-save (point-min) (point-max))
  (browse-url "https://ci.appveyor.com/tools/validate-yaml"))

(defun nvp-yaml-validate (arg)
  "Validate yaml: for travis use local linter, appveyor check on web."
  (interactive "P")
  (nvp-yaml-execute
   arg '((travis   . nvp-yaml-travis-lint)
         (appveyor . nvp-yaml-appveyor-validate))))

;; -------------------------------------------------------------------
;;; Online help

(defun nvp-yaml-info (arg)
  "Lookup help docs for travis / appveyor online."
  (interactive "P")
  (nvp-yaml-execute
   arg
   '((travis   . (browse-url "https://docs.travis-ci.com"))
     (appveyor . (browse-url "https://www.appveyor.com/docs/appveyor-yml/")))))

(defsubst nvp-yaml--url-format (type pname &optional arg)
  (cond
   ((eq type :travis)
    (format "https://travis-ci.org/%s"
            (if arg "profile/nverno" (concat "nverno/" pname))))
   ((eq type :appveyor)
    (format "https://ci.appveyor.com/project%s"
            (if arg "s" (concat "/nverno/" pname))))
   (t (user-error "unsupported type"))))

;; goto travis page for project, or profile with prefix arg
(defun nvp-yaml-project-online (type &optional arg)
  (let* ((pname (nvp-yaml-project-name))
         (repo (nvp-yaml--url-format type pname arg)))
    (browse-url repo)))

;; open project url, or with prefix arg open profile projects page
(defun nvp-yaml-project-info (arg)
  "Open project URL or profile project page with prefix ARG."
  (interactive "P")
  (nvp-yaml-execute
   nil `((travis   . (nvp-yaml-project-online :travis ,arg))
         (appveyor . (nvp-yaml-project-online :appveyor ,arg)))))

(provide 'nvp-yaml)
;;; nvp-yaml.el ends here
