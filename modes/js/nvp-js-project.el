;;; nvp-js-project.el --- project configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; TODO: Project setup, configuration, running, testing, and compiling
;;
;; Need support for project types:
;; - React - create-react-app boostrapped projects
;; - Node/npm
;; - yarn
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
;; - travis runner?
;;
;; Run => start development environment
;; Test: only worry about jest framework
;;   + need mappings b/w files
;;   + test runner script in package.json
;;   + could have option for coverage
;;   + would be nice to have output with XREFs to failures
;;   + Also, ability to run specific unit tests/groups of tests with regexps or
;;     something similar
;; Compile:
;;   + Build project to produce static site
;;   + Deploy somehow?
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp-decls)

;; -------------------------------------------------------------------
;;; Configuration
(nvp-defvar nvp-js-test-re (regexp-opt '("jest" "mocha" "jasmine") t))

;; set local values, eg. in .dir-locals.el
(defun nvp-js-local-config (&optional dir)
  (when-let ((default-directory
               (nvp-project-root
                (or dir buffer-file-name default-directory))))
    (nvp-async-shell-command-to-string
     "npm list --depth=0 --only=dev --parseable | awk -F/ '{print $NF}'"
     `(lambda (p res)
        (let ((default-directory ,default-directory))
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
              (kill-buffer (process-buffer p)))))))))

(provide 'nvp-js-project)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-js-project.el ends here
