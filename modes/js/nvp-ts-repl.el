;;; nvp-ts-repl.el --- Typescript REPL -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; > npm i ts-node typescript
;; and this should setup ts-comint to work properly with node module binary
;;; Code:

(eval-when-compile (require 'nvp-macro))
(require 'ts-comint nil t)
(unless (featurep 'nvp-ts) (require 'nvp-ts))
(nvp:decls :p (ts add-node) :v (ts-comint-buffer nvp-typescript-modes) :f (run-ts))

(defun nvp-ts-repl-program ()
  (when (require 'add-node-modules-path nil t)
    ;; npm v >= 9 no longer has 'npm bin' command
    (when (version< (string-trim-right (shell-command-to-string "npm -v")) "9")
      (nvp:setq add-node-modules-path-command '("npm bin")))
    (add-node-modules-path))
  (cl-some (lambda (program) (executable-find program)) '("ts-node" "tsun")))

;; Note: install `npm i ts-node typescript' in project and this
;; should find the local REPL binary
;;;###autoload
(defun nvp-ts-repl-setup (&optional force)
  (unless (or (null force)
              (and ts-comint-program-command
                   (executable-find ts-comint-program-command)))
    (setq ts-comint-program-command (nvp-ts-repl-program))))

(with-eval-after-load 'nvp-repl
  (when (fboundp 'run-ts)
    (nvp-repl-add nvp-typescript-modes
      :name 'typescript
      :modes '(ts-comint-mode)
      :bufname (regexp-quote "*Typescript")
      :send-string #'ts-send-string
      :send-region #'ts-send-region
      :send-sexp #'ts-send-last-sexp
      :send-buffer #'ts-send-buffer
      :send-file #'ts-load-file
      :history-file ".ts_history"
      :init (lambda (&optional _prefix)
              (nvp-ts-repl-setup 'force)
              (run-ts nil 'no-switch)
              (get-buffer-process ts-comint-buffer)))))

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
