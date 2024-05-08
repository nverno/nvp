;;; nvp-lua.el --- lua extensions -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'treesit)
(when (require 'lua-ts-mode nil t)
  (require 'nvp-lua-ts))

(nvp:decls
 :p (lua) :f (lua-funcname-at-point json-read-object)
 :v (lua-documentation-function lua-documentation-url))


(defvar nvp-lua--dir (file-name-directory (nvp:load-file-name)))

(with-eval-after-load 'nvp-repl (require 'nvp-lua-repl))

;; Syntax table used when looking identifier at point
(defvar nvp-lua-help-syntax-table
  (let ((tab (copy-syntax-table lua-ts--syntax-table)))
    (modify-syntax-entry ?. "_" tab)
    tab))

(defun nvp-lua-thing-at-point (&rest _)
  (with-syntax-table nvp-lua-help-syntax-table
    (thing-at-point 'symbol t)))

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


;;; Abbrevs
(defvar nvp-lua--top-level-funcs-query
  (when (treesit-available-p)
    (treesit-query-compile
     'lua
     '((chunk (function_declaration name: (_) @function))
       ;; (field name: (_) @function
       ;;        value: (function_definition))
       (chunk
        (variable_declaration
         (assignment_statement
          (variable_list
           name: (_) @named-closure)
          (expression_list
           value: (function_definition)))))))))

;;; XXX(5/8/24): handle `:local' arg?
(cl-defmethod nvp-parse-functions
  (&context (major-mode lua-ts-mode) &rest _args)
  (--map (treesit-node-text it t)
         (treesit-query-capture
          (treesit-buffer-root-node 'lua) nvp-lua--top-level-funcs-query
          nil nil t)))

(cl-defmethod nvp-abbrevd-make-args
  (&context (major-mode lua-ts-mode) &rest args)
  "Return functions to abbrev.

If non-nil, \\=':replace-table is a cons of a table name and a string to
replace it with in abbrevs. For example, \\='(cons \"M\" \"filetype\")
means replace \"M.some_fun\" \"filetype.some_fun\" in abbrevs.

By default, methods and functions prefixed with \"_\" are ignored."
  (when-let ((funcs (nvp-parse-functions)))
    (pcase-let ((`(,table . ,rep) (plist-get args :replace-table)))
      (when table
        (setq table (concat table ".")
              rep (concat rep ".")))
      (list :transformer #'nvp-abbrev-from-words
            :objects
            (cl-loop for def in funcs
                     unless (string-match-p (rx (or ":" (seq bos "_"))) def)
                     collect (if (and table (string-prefix-p table def))
                                 (concat rep (substring def (length table)))
                               def))))))

(provide 'nvp-lua)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-lua.el ends here
