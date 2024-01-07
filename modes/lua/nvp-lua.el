;;; nvp-lua.el --- lua extensions -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(when (require 'lua-ts-mode nil t)
  (require 'nvp-lua-ts))
(nvp:decls :p (lua))

(defvar nvp-lua--dir (file-name-directory (nvp:load-file-name)))

;;; REPL
(with-eval-after-load 'nvp-repl
  (require 'nvp-lua-repl))

;;; Fold
(defvar hs-special-modes-alist)
(with-eval-after-load 'hideshow
  (let ((rules '("\\(?:local \\)?function\\|if\\|do\\|while\\|for\\|{\\|\\[\\["
                 "end\\|}\\|\\]\\]" "--" nil)))
    (dolist (mode '(lua-ts-mode lua-mode))
      (if (assq mode hs-special-modes-alist)
          (setf (cdr (assq 'lua-ts-mode hs-special-modes-alist)) rules)
        (push `(,mode ,@rules) hs-special-modes-alist)))))

;;; Snippets
(nvp:decl nvp-yas-split-args yas-field-value)

;; return keys from 'key1 [= val1][, key_i [= val_i]]*'
(defun nvp-lua--keys (&optional str)
  (nvp-yas-split-args (or str yas-text) "[ \t]*=[^,]*,?[ \t]*"))

;; produce "-" when range is decreasing from INIT to LIMIT
(defun nvp-lua--step-sign (init limit)
  (let ((vinit (yas-field-value init))
        (vlim (yas-field-value limit)))
    (when (and vinit vlim
               (string-match-p "^[0-9.-]+" vlim)
               (> (string-to-number vinit) (string-to-number vlim)))
      "-")))

;;; Toggle
(defun nvp-lua-toggle-local ()
  "Toggle local for current function or assignment expression."
  (interactive)
  (let ((node (treesit-node-at (point))))
    (if (treesit-node-match-p node "local")
        (progn (delete-region (treesit-node-start node) (treesit-node-end node))
               (indent-according-to-mode))
      (--when-let 
          (treesit-parent-until
           node
           (lambda (n)
             (treesit-node-match-p
              n (rx (or "assignment_statement" "function_declaration")))))
        (save-excursion
          (goto-char (treesit-node-start it))
          (if (pcase (treesit-node-type it)
                ("assignment_statement"
                 (looking-back "\\_<local\\_>\\s-+" (line-beginning-position)))
                ("function_declaration"
                 (looking-at "\\_<local\\_>\\s-*"))
                (_ nil))
              (delete-region (match-beginning 0) (match-end 0))
            (insert "local ")))))))

;;; Manual
(defvar nvp-lua--manual-sections nil)

(defun nvp-lua-manual-sections ()
  "Populate the Lua manual section cache."
  (or nvp-lua--manual-sections
      (nvp:with-process "python"
        :proc-name "lua-manual"
        :proc-args ((expand-file-name "bin/manual-links.py" nvp-lua--dir))
        :on-success (progn (goto-char (point-min))
                           (setq nvp-lua--manual-sections (json-read-object))
                           (kill-buffer)))))

;; better version of `lua-search-documentation' for lua-mode
(defun lua-search-documentation (section)
  "Search Lua documentation for the word at the point."
  (interactive
   (let ((func (lua-funcname-at-point)))
     (if (and func (not current-prefix-arg)) (list func)
       (list (--if-let (nvp-lua-manual-sections)
                 (completing-read "Lua manual section: " it nil nil func)
               (read-string "Lua manual section(building...): " func))))))
  (funcall lua-documentation-function
           (concat lua-documentation-url "#pdf-" section)))

(provide 'nvp-lua)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-lua.el ends here
