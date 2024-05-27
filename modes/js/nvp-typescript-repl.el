;;; nvp-typescript-repl.el --- Typescript REPL -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; > npm i ts-node typescript
;; and this should setup ts-comint to work properly with node module binary
;;; Code:

(eval-when-compile (require 'nvp-macro))
(require 'nvp-repl)
(nvp:decls :p (ts add-node) :v (ts-repl-buffer nvp-typescript-modes) :f (ts-repl-run))

;; (defvar ts-comint-drop-regexp
;;   "\\(\x1b\\[[0-9]+[GJK]\\|^[ \t]*undefined[\r\n]+\\)"
;;   "Regex to silence matching output.")

;; FIXME: doesn't handle multi-line echoed input
;; (defun nvp-typescript-repl--preoutput-filter (string)
;;   "Filter extra escape sequences from STRING."
;;   (if (string= "> " string) ""
;;     (if (string-prefix-p "undefined" string)
;;         (substring string (length "undefined"))
;;       string))
;;   ;; (let ((beg (or comint-last-output-start
;;   ;;                (point-min-marker)))
;;   ;;       (end (process-mark (get-buffer-process (current-buffer)))))
;;   ;;   (save-excursion
;;   ;;     (goto-char beg)
;;   ;;     ;; Remove ansi escape sequences used in readline.js
;;   ;;     (while (re-search-forward ts-comint-drop-regexp end t)
;;   ;;       (replace-match ""))))
;;   )

(defvar nvp-typescript--repl-commands
  '("break" "clear" "exit" "help" "save" "load" "editor" "type"))

(when (fboundp 'ts-repl-run)
  (require 'ts-repl)
  (nvp-repl-add nvp-typescript-modes
    :name 'typescript
    :modes '(ts-repl-mode)
    :init #'ts-repl-run
    :find-fn #'ts-repl-process
    ;; :send-string #'ts-send-string
    ;; :send-region #'ts-send-region
    ;; :send-sexp #'ts-send-last-sexp
    ;; :send-buffer #'ts-send-buffer
    ;; :send-file #'ts-load-file
    :commands nvp-typescript--repl-commands
    :cmd-prefix ?.
    :help-cmd '(:no-arg ".help" :with-arg ".type %s")
    :pwd-cmd "process.cwd()"
    :cd-cmd "process.chdir(\"%s\")"
    :cmd-handlers '(("?" . ".type %s"))
    :history-file ".ts_history"))

(defun nvp-typescript-repl-get-file-mod (filename)
  (concat "import * as "
          (replace-regexp-in-string
           "^[0-9_]+" ""
           (replace-regexp-in-string "-" "_" (file-name-base filename)))
          " from \""
          (file-name-base filename)
          "\"\n"))

;;; FIXME: remove
(advice-add 'ts-comint--get-load-file-cmd :override #'nvp-typescript-repl-get-file-mod)

(provide 'nvp-typescript-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-typescript-repl.el ends here
