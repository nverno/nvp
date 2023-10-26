;; julia-ext.el --- -*- no-byte-compile: t; lexical-binding: t; -*-

;; Remove all extra dependencies

;;; Code:

(require 'ess-comp)
(defvar julia-ext-basic-offset)

(autoload 'ess-mode     "ess-mode" "Edit an ESS process.")

;; ------------------------------------------------------------
;;* Help

(defun julia-ext-get-help-topics (&optional proc)
  (append (with-current-buffer (ess-command "ESS.all_help_topics()\n")
            (split-string (buffer-string) "\n"))
          (julia-ext--get-objects proc)))

(defun julia-ext--retrive-topics (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (require 'url)
    (goto-char (point-min))
    (let (out)
      (while (re-search-forward "toctree.*href=\"\\(.+\\)\">\\(.+\\)</a" nil t)
        (push (propertize (match-string 2)
                          :manual (concat url (match-string 1)))
              out))
      (kill-buffer)
      (nreverse out))))

(defvar julia-ext--manual-topics nil)
(defun julia-ext-manual-lookup-function (&rest args) ; args are not used
  (interactive)
  "Look up topics at http://docs.julialang.org/en/latest/manual/"
  ;; <li class="toctree-l1"><a class="reference internal" href="introduction/">Introduction</a></li>
  (let* ((pages (or julia-ext--manual-topics
                    (setq julia-ext--manual-topics
                          (julia-ext--retrive-topics "http://docs.julialang.org/en/latest/manual/"))))
         (page (ess-completing-read "Lookup:" pages nil t)))
    (browse-url (get-text-property 1 :manual page))))

;; julia 0.3.0 doesn't provide categories. Thus we don't support this anymore.
;; (defun julia-ext-reference-lookup-function (&rest args) ; args are not used
;;   (interactive)
;;   "Look up reference topics"
;;   ;; <li class="toctree-l1"><a class="reference internal" href="introduction/">Introduction</a></li>
;;   (let* ((pages (ess-get-words-from-vector "ESS.help_categories()\n")))
;;     (ess-display-help-on-object
;;      (ess-completing-read "Category" pages nil t))))

;; ------------------------------------------------------------
;;* Errors

(defvar julia-ext-error-regexp-alist
  '((julia-ext-in  "^\\s-*in [^ \t\n]* \\(at \\(.*\\):\\([0-9]+\\)\\)" 2 3 nil 2 1)
    (julia-ext-at "^\\S-+\\s-+\\(at \\(.*\\):\\([0-9]+\\)\\)"  2 3 nil 2 1)
    (julia-ext-while-load "^\\s-*\\(while loading\\s-\\(.*\\), in .* on line +\\([0-9]+\\)\\)"  2 3 nil 2 1))
  "List of symbols which are looked up in `compilation-error-regexp-alist-alist'.")

(defun julia-ext-compile-setup ()
  "Add julia compile regexps."
  (mapcar (lambda (item)
            (push (car item) compilation-error-regexp-alist)
            (push item compilation-error-regexp-alist))
          julia-ext-error-regexp-alist))

(eval-after-load 'compile
  '(julia-ext-compile-setup))

;; ------------------------------------------------------------
;;* Imenu

(defvar julia-ext-imenu-generic-expression
  ;; don't use syntax classes, screws egrep
  '(("Function (_)" "[ \t]*function[ \t]+\\(_[^ \t\n]*\\)" 1)
    ("Function" "^[ \t]*function[ \t]+\\([^_][^\t\n]*\\)" 1)
    ("Const" "[ \t]*const \\([^ \t\n]*\\)" 1)
    ("Type"  "^[ \t]*[a-zA-Z0-9_]*type[a-zA-Z0-9_]* \\([^ \t\n]*\\)" 1)
    ("Require"      " *\\(\\brequire\\)(\\([^ \t\n)]*\\)" 2)
    ("Include"      " *\\(\\binclude\\)(\\([^ \t\n)]*\\)" 2)
    ))

;; ------------------------------------------------------------
;;* Major Mode

(defvar julia-ext-customize-alist
  '((comint-use-prompt-regexp      . t)
    (ess-eldoc-function            . 'julia-ext-eldoc-function)
    (inferior-ess-primary-prompt   . "a> ") ;; from julia>
    (inferior-ess-secondary-prompt . nil)
    (inferior-ess-prompt           . "\\w*> ")
    (ess-local-customize-alist     . 'julia-ext-customize-alist)
    (inferior-ess-program          . inferior-julia-program-name)
    (ess-get-help-topics-function  . 'julia-ext-get-help-topics)
    (ess-help-web-search-command   . "http://docs.julialang.org/en/latest/search/?q=%s")
    (ess-manual-lookup-command     . 'julia-ext-manual-lookup-function)
    ;; (ess-reference-lookup-command       . 'julia-ext-reference-lookup-function)
    (ess-load-command              . "include(\"%s\")\n")
    (ess-funargs-command           . "ESS.fun_args(\"%s\")\n")
    (ess-dump-error-re             . "in \\w* at \\(.*\\):[0-9]+")
    (ess-error-regexp              . "\\(^\\s-*at\\s-*\\(?3:.*\\):\\(?2:[0-9]+\\)\\)")
    (ess-error-regexp-alist        . julia-ext-error-regexp-alist)
    (ess-imenu-generic-expression  . julia-ext-imenu-generic-expression)
    (ess-mode-syntax-table         . julia-mode-syntax-table)
    (ess-mode-completion-syntax-table . julia-ext-completion-syntax-table)
    ;; (inferior-ess-objects-command    . inferior-R-objects-command)
    ;; (inferior-ess-search-list-command        . "search()\n")
    (inferior-ess-help-command     . "ESS.help(\"%s\")\n")
    ;; (inferior-ess-help-command       . "help(\"%s\")\n")
    (ess-language                  . "julia")
    (ess-dialect                   . "julia")
    (ess-suffix                    . "jl")
    (ess-ac-sources                . '(ac-source-julia-ext-objects))
    (ess-company-backends          . '(company-julia-ext-objects))
    (ess-dump-filename-template    . (ess-replace-regexp-in-string
                                      "S$" ess-suffix ; in the one from custom:
                                      ess-dump-filename-template-proto))
    (ess-mode-editing-alist        . nil)
    (ess-change-sp-regexp          . nil );ess-R-change-sp-regexp)
    (ess-help-sec-regex            . ess-help-R-sec-regex)
    (ess-help-sec-keys-alist       . ess-help-R-sec-keys-alist)
    (ess-loop-timeout              . ess-S-loop-timeout);fixme: dialect spec.
    (ess-cmd-delay                 . ess-R-cmd-delay)
    (ess-function-pattern          . ess-R-function-pattern)
    (ess-object-name-db-file       . "ess-jl-namedb.el" )
    (ess-smart-operators           . ess-R-smart-operators)
    (inferior-ess-help-filetype    . nil)
    (inferior-ess-exit-command     . "exit()\n")
    ;;harmful for shell-mode's C-a: -- but "necessary" for ESS-help?
    (inferior-ess-start-file       . nil) ;; "~/.ess-R"
    (inferior-ess-start-args       . "")
    (inferior-ess-language-start   . nil)
    (ess-STERM                     . "iESS")
    (ess-editor                    . R-editor)
    (ess-pager                     . R-pager)
    (ess-getwd-command             . "pwd()\n")
    (ess-setwd-command             . "cd(expanduser(\"%s\"))\n")
    )
  "Variables to customize for Julia -- set up later than emacs initialization.")

(defvar julia-ext-completion-syntax-table
  (let ((table (make-syntax-table ess-r-syntax-table)))
    (modify-syntax-entry ?. "_" table)
    ;; (modify-syntax-entry ?: "_" table)
    ;; (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?@ "_" table)
    table)
  "Syntax table used for completion and help symbol lookup.
It makes underscores and dots word constituent chars.")

;;;###autoload
(define-derived-mode julia-ext-mode julia-mode "ESS[julia]"
  "Major mode for editing julia source.  See `ess-mode' for more help."
  (ess-mode julia-ext-customize-alist nil t)
  ;; for emacs >= 24
  (remove-hook 'completion-at-point-functions 'ess-filename-completion 'local) ;; should be first
  (add-hook 'completion-at-point-functions 'julia-ext-object-completion nil 'local)
  (add-hook 'completion-at-point-functions 'ess-filename-completion nil 'local)
  (if (fboundp 'ess-add-toolbar) (ess-add-toolbar))
  (set (make-local-variable 'end-of-defun-function) 'ess-end-of-function)

  (set (make-local-variable 'julia-ext-basic-offset) 4)
  (setq imenu-generic-expression julia-ext-imenu-generic-expression)
  (imenu-add-to-menubar "Imenu-jl"))

(defvar julia-ext-post-run-hook nil
  "Functions run in process buffer after starting julia process.")

;; (add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-ext-mode) t)

(provide 'julia-ext)
;;; julia-ext.el ends here
