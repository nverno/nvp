;;; nvp-ts-repl.el --- Typescript REPL -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; > npm i ts-node typescript
;; and this should setup ts-comint to work properly with node module binary
;;; Code:

(eval-when-compile (require 'nvp-macro))
(require 'ts-comint nil t)
(require 'nvp-repl)
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
  (if (null force) ts-comint-program-command
    (or (and ts-comint-program-command
             (executable-find ts-comint-program-command))
        (setq ts-comint-program-command (nvp-ts-repl-program)))))

;; From `js-comint'
(defvar ts-comint-drop-regexp
  "\\(\x1b\\[[0-9]+[GJK]\\|^[ \t]*undefined[\r\n]+\\)"
  "Regex to silence matching output.")

;; FIXME: doesn't handle multi-line echoed input
(defun ts-comint-filter-output (_string)
  "Filter extra escape sequences from STRING."
  (let ((beg (or comint-last-output-start
                 (point-min-marker)))
        (end (process-mark (get-buffer-process (current-buffer)))))
    (save-excursion
      (goto-char beg)
      ;; Remove ansi escape sequences used in readline.js
      (while (re-search-forward ts-comint-drop-regexp end t)
        (replace-match "")))))

(defun nvp-ts-repl-init (&optional _prefix)
  (interactive "P")
  (or (nvp-ts-repl-setup 'force)
      (user-error "No ts repl found (need to npm i ts-node?)"))
  (run-ts nil 'no-switch)
  (with-current-buffer ts-comint-buffer
    (setq-local comint-prompt-regexp "^> ")
    (add-hook 'comint-output-filter-functions 'ts-comint-filter-output nil t)
    (get-buffer-process (current-buffer))))

(when (fboundp 'run-ts)
  (nvp-repl-add nvp-typescript-modes
    :name 'typescript
    :modes '(ts-comint-mode)
    :bufname (regexp-quote "*Typescript")
    :init #'nvp-ts-repl-init
    ;; :send-string #'ts-send-string
    :send-region #'ts-send-region
    :send-sexp #'ts-send-last-sexp
    :send-buffer #'ts-send-buffer
    :send-file #'ts-load-file
    :help-cmd '(:no-arg ".help" :with-arg ".type %s")
    :history-file ".ts_history"))

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
