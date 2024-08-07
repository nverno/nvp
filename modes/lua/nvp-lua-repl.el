;;; nvp-lua-repl.el --- Lua REPL -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-repl)
(require 'inf-lua nil t)
(nvp:decls :p (inf-lua lua) :f (inf-lua-run))


(with-eval-after-load 'inf-lua
  (nvp-repl-add '(lua-mode lua-ts-mode)
    :name 'lua
    :modes '(inf-lua-mode)
    :init #'nvp-lua-repl-init
    :find-fn #'inf-lua-process
    :history-file ".lua_history"
    :cd-cmd "lfs=require 'lfs'; lfs.chdir(\"%s\")"
    :pwd-cmd "lfs=require 'lfs'; print(lfs.currentdir())"
    :help-cmd #'nvp-lua-repl-help
    :send-region #'nvp-lua-send-region
    :eval-output-filter #'nvp-lua-repl--eval-output-filter
    :eval-input-filter #'nvp-lua-repl--eval-input-filter
    ;; :eval-sexp #'nvp-lua-repl-eval-sexp
    :cmd-handlers '(("?"  . nvp-lua-repl-help)
                    ("pp" . "pp(%s)"))))


(defun nvp-lua-repl-help (&optional thing _proc)
  "Lookup docs for THING."
  (if (fboundp 'devdocs-lookup)
      (prog1 t (funcall-interactively #'devdocs-lookup nil thing))
    thing))

;;; Eval
(defun nvp-lua-repl--eval-output-filter (str)
  (replace-regexp-in-string inf-lua-prompt-continue "" str))

(defun nvp-lua-repl--eval-input-filter (str)
  (if (string-match-p "\\`\\s-*return" str)
      str
    (concat "return " str)))

(defun nvp-lua-repl-eval-sexp (&optional _insert)
  "Eval sexp-like thing near point."
  (interactive "P")
  (let ((bnds (if (region-active-p)
                  (car (region-bounds))
                (bounds-of-thing-at-point 'sentence)))
        str)
    (unless bnds
      (user-error "cant find sexp"))
    (nvp-indicate-pulse-region-or-line (car bnds) (cdr bnds))
    (setq str (buffer-substring-no-properties (car bnds) (cdr bnds)))
    ;; XXX(6/29/24): choose better when to add 'return'
    (unless (string-match-p "\\`\\s-*return" str)
      (setq str (concat "return " str)))
    str
    ;; (nvp-repl-send-string str _insert)
    ;; (nvp-repl-show-result nil _insert)
    ))

(defun nvp-lua-repl-init (&optional prefix)
  "Launch lua repl.
With single PREFIX arg setup for debugger:
 - doesnt load init file
 - enables compilation minor mode
With two \\[universal-argument] prompt for lua command."
  (let* ((process-environment           ; Linenoise is useless in emacs
          (append (list "DBG_NOREADLINE=1")
                  (copy-sequence process-environment)))
         (proc (funcall #'inf-lua-run (equal '(16) prefix) nil
                        (and nvp-repl-load-startup-file
                             (or (null prefix)
                                 (>= (prefix-numeric-value prefix) 16))
                             inf-lua-startfile))))
    (process-send-string proc (concat lua-process-init-code "\n"))
    proc))

(defvar lua-process-init-code
  (mapconcat
   'identity
   '("local loadstring = loadstring or load"
     "function __REPL_loadstring(str, displayname, lineoffset)"
     "  if lineoffset > 1 then"
     "    str = string.rep('\\n', lineoffset - 1) .. str"
     "  end"
     ""
     "  local x, e = loadstring(str, '@'..displayname)"
     "  if e then"
     "    error(e)"
     "  end"
     "  return x()"
     "end")
   " ")
  "Code from `lua-mode' to add source location info to strings.")

(defun nvp-lua-send-region (beg end)
  "Send region from BEG to END to Lua repl."
  (interactive "r")
  (let ((text (buffer-substring-no-properties beg end)))
    (funcall #'nvp-repl-send-string
             (if (bound-and-true-p nvp-repl-eval-input-filter)
                 text
               (format "__REPL_loadstring(%s, %s, %s);\n"
                       (inf-lua-string-to-literal text)
                       (inf-lua-string-to-literal (or (buffer-file-name)
                                                      (buffer-name)))
                       (line-number-at-pos beg))))))


(provide 'nvp-lua-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-lua-repl.el ends here
