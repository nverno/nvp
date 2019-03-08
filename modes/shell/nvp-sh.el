;;; nvp-sh.el --- sh script helpers -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/shell-tools
;; Last modified: <2019-03-08 05:32:59>
;; Package-Requires: 
;; Created:  5 December 2016

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; TODO:
;;   - somehow merge local variable completion with bash-completion
;;   - restrict local variable candidates to the current context + globals
;;   - fix narrowing
;;   - create docstrings from function headers
;;   - jump to sourced functions
;;   - don't leave sourced scripts sitting around in buffers

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (require 'subr-x)
  (defvar explicit-shell-file-name))
(require 'nvp)
(require 'nvp-parse)
(require 'nvp-shell)
(require 'nvp-sh-help)
(require 'company)
(require 'company-quickhelp)
(require 'company-bash)
(require 'sh-script)

(declare-function company-shell "company-shell")
(declare-function bash-completion-dynamic-complete "bash-completion")
(declare-function bash-completion-dynamic-complete-nocomint "bash-completion")

(autoload 'string-trim "subr-x")

;; -------------------------------------------------------------------
;;; Variables

;; for jumping b/w functions -- see `sh-imenu-generic-expression'
(defvar nvp-sh-function-re
  (nvp-concat
   "\\(?:"
   ;; function FOO()
   "^\\s-*function\\s-+\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*\\(?:()\\)?"
   "\\|"
   ;; FOO()
   "^\\s-*\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*()"
   "\\)"))

;; imenu header comment regexp
(defvar nvp-sh-comment-headers-re '((nil "^###\\s-*\\(.+\\)\\s-*$" 1)))

;; additional imenu regexps
(defvar nvp-sh-imenu-extra-regexps
  '(("Sources" "^\\(?:\\\.\\|source\\)\\s-+\\(.+\\)\\s-*$" 1)
    ("Globals" "^\\([A-Za-z_][^=\n]*\\)=" 1)))

;; -------------------------------------------------------------------
;;; Utils

;; true if point is in heredoc
(defsubst nvp-sh--here-doc-p (point)
  (eq (get-text-property point 'face) 'sh-heredoc))

;; get the here doc marker for block
(defsubst nvp-sh--here-doc-marker (&optional point)
  (let ((ppss (syntax-ppss point)))
    (when (eq t (nth 3 ppss))
      (get-text-property (nth 8 ppss) 'sh-here-doc-marker))))

;; position at beginning of first line of here-doc if point is
;; currently in a here-doc
(defun nvp-sh-here-doc-start-pos (point)
  (save-excursion
    (goto-char point)
    (back-to-indentation)
    (when (looking-back "[^<]<<.*" (line-beginning-position)) ;skip opening line
      (forward-line))
    (let ((in-heredoc (nvp-sh--here-doc-p (point))))
      (when in-heredoc                      ;search back until no marker
        (while (and (not (bobp))
                    (nvp-sh--here-doc-p (point)))
          (forward-line -1)
          (back-to-indentation))
        (point)))))

(defun nvp-sh-narrow-lexically ()
  "Like `narrow-to-defun', but only narrow if point is actually inside a function.
Retrun point at start of function if narrowing was done."
  (save-excursion
    (widen)
    (when-let* ((ppss (syntax-ppss))
                (parens (nth 9 ppss)))   ;start of outermost parens
      (goto-char (car parens))
      (and (eq (char-after) ?{)
           (progn
             (narrow-to-region (point) (progn (forward-sexp) (point)))
             (car parens))))))

(cl-defmethod nvp-parse-current-function (&context (major-mode sh-mode) &rest _args)
  "Find name of function containing point.
Like `sh-current-defun-name' but ignore variables."
  (save-excursion
    (end-of-line)
    (when (re-search-backward nvp-sh-function-re nil 'move)
      (or (match-string-no-properties 1)
          (match-string-no-properties 2)))))

;; -------------------------------------------------------------------
;;; Navigation
;; commands to enable `beginning-of-defun', `end-of-defun', `narrow-to-defun',
;; etc. to work properly in sh buffers

(defsubst nvp-sh-looking-at-beginning-of-defun ()
  (save-excursion
    (beginning-of-line 1)
    (looking-at-p nvp-sh-function-re)))

(defun nvp-sh--beginning-of-defun (&optional arg)
  "Internal implementation for function navigation.
With positive ARG search backwards."
  (when (or (null arg) (= arg 0)) (setq arg 1))
  (let ((search-fn (if (> arg 0)
                       #'re-search-backward
                     #'re-search-forward))
        (pos (point-marker)))
    (and (< arg 0)                          ;searching forward -- skip current func
         (nvp-sh-looking-at-beginning-of-defun)
         (end-of-line 1))
    (funcall search-fn nvp-sh-function-re nil 'move)
    (if (nvp-sh-looking-at-beginning-of-defun)
        (or (beginning-of-line 1) (point))  ;found function
      (and (goto-char pos) nil))))          ;failed to find one

(defun nvp-sh-beginning-of-defun (&optional arg)
  "Move to beginning of defun.
With positive ARG search backwards, otherwise forwards.
Used to set `beginning-of-defun-function'."
  (when (or (null arg) (= arg 0)) (setq arg 1))
  (let (found)
    (while (and (not (= arg 0))
                (let ((keep-searching-p (nvp-sh--beginning-of-defun arg)))
                  (when (and keep-searching-p (null found))
                    (setq found t))
                  keep-searching-p))
      (setq arg (if (> arg 0) (1- arg) (1+ arg))))
    found))

(defun nvp-sh-end-of-defun ()
  "Move point to end of current function.
Used to set `end-of-defun-function'."
  (when (or (nvp-sh-looking-at-beginning-of-defun) ;find beginning
            (nvp-sh-beginning-of-defun 1)
            (nvp-sh-beginning-of-defun -1))
    (beginning-of-line)
    (when (search-forward "{" nil 'move) ;move to opening '{' and jump sexp
      (forward-char -1)
      (forward-list)
      (point))))

;; ------------------------------------------------------------
;;; Completion

(defvar nvp-sh-company-backends '(company-bash :with company-shell))
(when (require 'bash-completion nil t)
  (setq nvp-sh-company-backends '(company-bash :with company-capf)))


(defun nvp-sh-vars-before-point ()
  "Collect variables in current lexical context."
  (save-excursion
    (if (not (string= sh-shell "bash"))
        (sh--vars-before-point)
      (let ((func-start (nvp-sh-narrow-lexically))
            vars)
        (widen)
        ;; collect local variables
        (while (re-search-backward "[ \t]*local[ \t]\\([^=\n]+\\)" func-start 'move)
          (dolist (var (split-string (match-string 1) " \t" 'omit " \t"))
            (push var vars)))
        vars))))

(defun nvp-sh-dynamic-complete-vars ()
  "Complete local variables, but fail if none match to delegate to bash completion."
  (nvp-unless-in-comment
    (save-excursion
        (skip-chars-forward "[:alnum:]_")
        (let ((end (point))
              (_ (skip-chars-backward "[:alnum:]_"))
              (start (point)))
          (when (or (eq (char-before) ?$)
                    (and (eq (char-before) ?{)
                         (eq (char-before (1- start)) ?$)))
            (list start end
                  (completion-table-dynamic
                   (lambda (s) (all-completions s (sh--vars-before-point))))
                  :exclusive 'no))))))

(defun nvp-sh-dynamic-complete-bash ()
  "Bash dynamic completion for sh-script (doesn't get local variables)."
  (nvp-unless-in-comment
    (bash-completion-dynamic-complete-nocomint
     (save-excursion (sh-beginning-of-command)) (point) 'dynamic-table)))

;; setup company backends with company-bash and either company-shell
;; or bash-completion
(defun nvp-sh-completion-setup ()
  (set (make-local-variable 'company-backends) (remq 'company-capf company-backends))
  (nvp-with-gnu
    (when (require 'bash-completion nil t)
      ;; use bash-completion + completion for variables / sourced functions
      (add-hook 'completion-at-point-functions 'nvp-sh-dynamic-complete-bash nil t)
      ;; allow completion of local variables as well
      (add-hook 'completion-at-point-functions 'nvp-sh-dynamic-complete-vars nil t)))
  ;; use local version of `company-active-map' to rebind
  ;; functions to show popup help and jump to help buffer
  (nvp-bindings "company-active-map" 'company :buff-local t
    ("M-h" . nvp-sh-quickhelp-toggle)
    ("C-h" . nvp-sh-company-show-doc-buffer))
  (cl-pushnew nvp-sh-company-backends company-backends)
  (setq-local company-transformers '(company-sort-by-backend-importance)))

;; with prefix, only complete for sourced / local functions
(defun nvp-sh-company-bash (arg)
  "Temporarily use only sourced / local functions for completion."
  (interactive "P")
  (if arg (call-interactively 'company-bash)
    (company-complete)))

;; -------------------------------------------------------------------
;;; Documentaion

;; Since no doc-buffer is returned by company-capf, rewrite
;; company-quickhelp doc retrieval method to just call man on the
;; current candidates
;; return a company-documentation buffer with either Man output or bash help
;; for a builtin
(defun nvp-sh-doc-buffer (cmd)
  (let ((doc-str (if (nvp-sh-help-bash-builtin-p cmd)
                     (nvp-sh-help-bash-builtin-sync cmd)
                   (shell-command-to-string (format "man %s" cmd)))))
    (and (not (or (member doc-str '(nil ""))
                  (string-prefix-p "No manual entry" doc-str)))
         (company-doc-buffer doc-str))))

(defun nvp-sh-quickhelp-doc (selected)
  (cl-letf (((symbol-function 'completing-read)
             #'company-quickhelp--completing-read))
    (let* ((doc-buff (nvp-sh-doc-buffer selected))
           (doc-and-meta
            (with-current-buffer doc-buff
              (company-quickhelp--docstring-from-buffer (point-min))))
           (truncated (plist-get doc-and-meta :truncated))
           (doc (plist-get doc-and-meta :doc)))
      (unless (member doc '(nil ""))
        (if truncated
            (concat doc "\n\n[...]")
          doc)))))

;; re-bind to this in `company-active-map'
(defun nvp-sh-quickhelp-toggle ()
  (interactive)
  (let ((x-gtk-use-system-tooltips nil)
        ;; (company-quickhelp-delay 0.1)
        )
    (or (x-hide-tip)
        (cl-letf (((symbol-function 'company-quickhelp--doc)
                   #'nvp-sh-quickhelp-doc))
          ;; flickers the screen - cant use the timer, since it seems
          ;; that lexical binding doesn't work in that case
          (company-quickhelp--show)))))

;; show help buffer in other window from company-active-map
(defun nvp-sh-company-show-doc-buffer ()
  (interactive)
  (cl-letf (((symbol-function 'company-call-backend)
             #'(lambda (_type selected)
                 (nvp-sh-doc-buffer selected) "*company-documentation*")))
    (company-show-doc-buffer)))

;; ------------------------------------------------------------
;;; Font-lock

;; Add additional font-locking to quoted variables
;; Non-nil if point in inside a double-quoted string.
(defsubst nvp-sh-font-lock--quoted-p ()
  (eq (nth 3 (syntax-ppss)) ?\"))

;; add fontification to double quoted stuff
(defun nvp-sh-fontify-quoted (regex limit)
  (let (res)
    (while (and (setq res (re-search-forward regex limit 'move))
                (not (nvp-sh-font-lock--quoted-p))))
    res))

(defun nvp-sh-font-lock ()
  "Add font-locking for variables, special variables, arrays and \
backquoted executables in double quotes."
  (font-lock-add-keywords
   nil
   `(                                   ;gaudy font-lock for array
     ("\\${\\([!#?]?[[:alpha:]_][[:alnum:]_]*\\[[@*]\\]\\)}"
      (1 'nvp-italic-variable-face prepend))
     ("\\([0-9&<>]*[ ]*/dev/null\\)" (1 'nvp-italic-type-face prepend))
     (,(apply-partially                 ;vars, special vars, function args
        #'nvp-sh-fontify-quoted
        "\\${?\\([[:alpha:]_][[:alnum:]_]*\\|[-#?@!*]\\|[0-9]\\)")
      (1 font-lock-variable-name-face prepend))
     (,(apply-partially                 ;functions in quoted `...`
        #'nvp-sh-fontify-quoted "`\\s-*\\([[:alnum:]_\\-]+\\)[^`]*`")
      (1 'sh-quoted-exec prepend))))
  (if (fboundp #'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (font-lock-ensure (point-min) (point-max)))))

;; -------------------------------------------------------------------
;;; REPL

;; replacement for `sh-shell-process'
(defun nvp-sh-get-process (&optional this-buffer)
  "Replacement for `sh-shell-process'.
Optionally return process specific to THIS-BUFFER."
  (let ((buffname (and this-buffer
                       (concat "*shell: " (buffer-name (current-buffer)) "*"))))
    (if (and (process-live-p sh-shell-process)
             (if buffname
                 (string= (buffer-name
                           (process-buffer sh-shell-process))
                          buffname)
               t))
       sh-shell-process
     (setq sh-shell-process
           (let ((proc (nvp-shell-get-process nil buffname)))
             (or proc
                 (get-buffer-process
                  (let ((explicit-shell-file-name sh-shell-file))
                    (shell buffname)))))))))

;; FIXME: do I always want a sh file to sending to its own shell?
(setf (symbol-function 'sh-shell-process) 'nvp-sh-get-process)

;; switch to shell REPL, specific to this buffer with a prefix arg
(nvp-repl-switch "sh"
    (:repl-mode 'shell-mode
     :repl-find-fn #'(lambda ()
                       (process-buffer (nvp-sh-get-process current-prefix-arg)))
     :repl-switch-fn 'pop-to-buffer)
  (process-buffer
   (setq sh-shell-process (nvp-sh-get-process current-prefix-arg))))

;; send selected region and step
(defun nvp-sh-send-region (beg end)
  "Send selected region from BEG to END to associated shell process."
  (interactive "r")
  (comint-send-string (nvp-sh-get-process)
                      (concat (buffer-substring beg end) "\n"))
  (goto-char end))

;; ------------------------------------------------------------
;;; Cleanup / Align

(require 'align)

;; alignment rules to align '\' not in strings/comments and
;; align end-of-line comments
(defvar nvp-sh-align-rules-list
  `((sh-line-continuation
     (regexp . "\\(\\s-*\\)\\\\\\s-*$")
     (modes  . '(sh-mode))
     (valid  . ,(function
                 (lambda () (save-excursion
                         (not (sh-in-comment-or-string (point))))))))
    (sh-eol-comments
     (regexp . "[^ #\t\n\\\\]\\(\\s-+\\)#+.*$")
     (group  . 1)
     (modes  . '(sh-mode))
     (valid  . ,(function
                 (lambda () (save-excursion
                         (goto-char (match-beginning 1))
                         (and (not (bolp))
                              (not (nth 3 (syntax-ppss)))))))))))

;; enforce uft-8-unix and align when killing buffer
(defun nvp-sh-cleanup-buffer ()
  (unless (or buffer-read-only (not (buffer-modified-p)))
    (align (point-min) (point-max))
    (and (buffer-modified-p)
         (save-buffer))))

;; -------------------------------------------------------------------
;;; Toggle

;; toggle here-doc indentation:
;; open with '<<-' and use only leading tabs for indentation
(defun nvp-sh-toggle-here-doc-indent (point)
  (interactive "d")
  (let ((start-pos (nvp-sh-here-doc-start-pos point)))
    (when start-pos
      (save-excursion
        (goto-char start-pos)
        (search-forward-regexp "[^<]<<" (line-end-position) 'move)
        (let ((indent (not (eq ?- (char-after))))
              marker)
          (if indent                    ;toggle preceding '-'
              (insert "-")
            (delete-char 1))
          (forward-to-indentation)      ;skip past opening line
          (setq marker (nvp-sh--here-doc-marker))
          (while (and (nvp-sh--here-doc-p (point))
                      (not (looking-at-p marker)))
            (delete-horizontal-space)
            (and indent                  ;toggle indentation
                 (insert "\t"))
            (forward-to-indentation)))))))

(defun nvp-sh-toggle-fontification ()
  "Toggle extra fontification on/off."
  (interactive)
  (nvp-toggled-if (font-lock-refresh-defaults)
    (nvp-sh-font-lock)))

;; -------------------------------------------------------------------
;;; Setup local variables

(defun nvp-sh-setup-hook ()
  "Define local variables to be called in hook."
  (setq-local beginning-of-defun-function 'nvp-sh-beginning-of-defun)
  (setq-local end-of-defun-function 'nvp-sh-end-of-defun)
  (setq-local align-rules-list nvp-sh-align-rules-list)
  (make-local-variable 'hippie-expand-try-functions-list)
  (push 'nvp-he-try-expand-shell-alias hippie-expand-try-functions-list)
  (nvp-sh-font-lock)
  (nvp-sh-completion-setup)
  (add-hook 'kill-buffer-hook 'nvp-sh-cleanup-buffer nil 'local))

(provide 'nvp-sh)
;;; nvp-sh.el ends here
