;;; nvp-lua-repl.el --- Lua REPL -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-repl)
(require 'inf-lua nil t)
(nvp:decls :p (inf-lua lua) :f (inf-lua-run))


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

(defun nvp-lua-repl--sender (proc str)
  "Function for `comint-input-sender'."
  (--if-let (string-match "^\\s-*\\?\\s-*\\(.+\\)?" str)
      (progn (comint-delete-input)
             (comint-simple-send proc "\n")
             (nvp-lua-repl-help (match-string 1 str)))
    (comint-simple-send proc str)))

(defun nvp-lua-repl-add-sender ()
  (setq-local comint-input-sender #'nvp-lua-repl--sender
              comint-input-history-ignore (rx bol (* white) (or "#" "?"))))
(add-hook 'inf-lua-mode-hook #'nvp-lua-repl-add-sender)

(defun nvp-lua-repl-help (&optional thing)
  (when (fboundp 'devdocs-lookup)
    (prog1 t (funcall-interactively #'devdocs-lookup nil thing))))

;; region sending from `lua-mode'
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
   " "))

(defun lua-make-lua-string (str)
  "Convert string to Lua literal."
  (save-match-data
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward "[\"'\\\t\\\n]" nil t)
        (cond ((string= (match-string 0) "\n")
               (replace-match "\\\\n"))
              ((string= (match-string 0) "\t")
               (replace-match "\\\\t"))
              (t
               (replace-match "\\\\\\&" t))))
      (concat "'" (buffer-string) "'"))))

;;; FIXME(5/12/24): implement `nvp-repl-send-string' function to add debug
;;; properties
(defun nvp-lua-send-region (beg end)
  (interactive "r")
  (setq beg (nvp-repl--skip-shebang beg))
  (nvp-repl-send-string
   (format "__REPL_loadstring(%s, %s, %s);\n"
           (lua-make-lua-string (buffer-substring-no-properties beg end))
           (lua-make-lua-string (or (buffer-file-name) (buffer-name)))
           (line-number-at-pos beg))))

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
    :eval-filter (lambda (s) (replace-regexp-in-string inf-lua-prompt-continue "" s))))

(provide 'nvp-lua-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-lua-repl.el ends here
