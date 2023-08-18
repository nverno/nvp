;;; nvp-ts-repl.el --- typescript REPL -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; > npm i ts-node typescript
;; and this should setup ts-comint to work properly with node module binary
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'ts-comint nil t)
(nvp:decls :v (ts-comint-buffer ts-comint-program-command)
           :f (ts-comint-mode run-ts add-node-modules-path))

;; Note: install `npm i ts-node typescript' in project and this
;; should find the local REPL binary
;;;###autoload
(defun nvp-ts-repl-setup (&optional force)
  (unless (or (null force)
              (and ts-comint-program-command
                   (executable-find ts-comint-program-command)))
    (setq ts-comint-program-command (nvp-ts-repl-program))))

(defun nvp-ts-repl-program ()
  (when (require 'add-node-modules-path nil t)
    ;; npm v >= 9 no longer has 'npm bin' command
    (when (version< (string-trim-right (shell-command-to-string "npm -v")) "9")
      (nvp:setq add-node-modules-path-command '("npm bin")))
    (add-node-modules-path))
  (cl-some (lambda (program) (executable-find program)) '("ts-node" "tsun")))

(defun nvp-ts-repl-get-file-mod (filename)
  (concat "import * as "
          (replace-regexp-in-string
           "^[0-9_]+" ""
           (replace-regexp-in-string "-" "_" (file-name-base filename)))
          " from \""
          (file-name-base filename)
          "\"\n"))

(advice-add 'ts-comint--get-load-file-cmd :override #'nvp-ts-repl-get-file-mod)


(provide 'nvp-ts-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-ts-repl.el ends here
