;;; nvp-js-project.el --- project configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configuration (work w/ projectile-configure-project?):
;; - local binary paths: ./node_modules/.bin/
;;   > npm (or yarn) bin [cmd] => will list path to local bin
;; - current npm/yarn configuration
;;   > npm/yarn config list
;; - .tern-project w/ webpack needs process-environment configured
;;   https://medium.com/@jrwillette88/tern-why-it-breaks-and-how-to-fix-it-8d1677df05f9
;;   this hack uses a tern shim to set env. vars -- but it would be better to
;;   configure the process-environment around the tern server.
;; - indium configuration, eg. chrome or node + debugging port, node --inspect
;;   command, etc., also needs to recognize correct sourcemaps
;; - .env / .env.development variables
;; - babel configuration (es6, jsx, ts) using preset-env, in config file
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls)

(defvar nvp-js-test-re (nvp:rx-syms "jest" "mocha" "jasmine"))

;; Get scripts from project package.json
(defun nvp-js--project-scripts ()
  (let ((pkg (expand-file-name "package.json" (nvp-project-root))))
    (when (file-exists-p pkg)
      (cdr (assq 'scripts (json-read-file pkg))))))

;; -------------------------------------------------------------------
;;; Configuration

;; set local values, eg. in .dir-locals.el
(defun nvp-js-local-config (&optional dir)
  (when-let* ((default-directory
              (nvp-project-root
               (or dir buffer-file-name default-directory))))
    (nvp-async-shell-command-to-string
     "npm list --depth=0 --only=dev --parseable | awk -F/ '{print $NF}'"
     (lambda (p _res)
       (let ((default-directory default-directory))
         (when (zerop (process-exit-status p))
           (unwind-protect
               (with-current-buffer (process-buffer p)
                 (goto-char (point-min))
                 (when (re-search-forward nvp-js-test-re nil t)
                   (let ((test (match-string 1)))
                     (save-window-excursion
                       (add-dir-local-variable nil 'nvp-test-framework test)
                       (save-buffer))
                     (message "Project: %s\nTest framework: %s"
                              (abbreviate-file-name default-directory)
                              test))))
             (kill-buffer (process-buffer p))))))
     "*js-config*")))

(provide 'nvp-js-project)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-js-project.el ends here
