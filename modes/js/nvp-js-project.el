;;; nvp-js-project.el --- project configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; TODO:
;; - need to setup local binary paths: ./node_modules/.bin/
;; - configure .tern-project locally when using webpack
;; - if .tern-project loads webpack.dev.js configured using create-react-app,
;;   it won't understand NODE_ENV, so environment variables need to be configured
;;   appropriately beforehand (or in worst case use a shim to start tern with
;;   environment variables -- this really shouldn't be required)
;; TODO: Projectile-related
;; - projectile-configure-project
;; - projectile-run-project
;; - projectile-test-project
;; - projectile-compile-project
;; Other:
;; - indium configuration
;; - local .tern-project configuration when necessary (webpack loader)
;; - enable appropriate snippets based on frameworks
;; - package.json reader to manage scripts/etc.
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
