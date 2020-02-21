;;; nvp-lisp.el --- lisp helpers  -*- lexical-binding: t; -*-
;;; Commentary:
;; TODO:
;; - auto-abbrevs
;; - sbcl: jump to source
;; - hap
;; - pophelp
;; - info
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'slime)
(nvp-decls :f (slime-repl-eval-string
               slime-repl-buffer slime-switch-to-output-buffer
               slime-connection-output-buffer slime-output-buffer
               slime-fuzzy-completions
               yas-hippie-try-expand)
           :v (slime-repl-input))
(nvp-auto "nvp-elisp" 'nvp-elisp-abbrev-expand-var-p 'nvp-elisp-abbrev-expand-fn-p)

;; return list of available lisps and their arguments
(defun nvp-lisp-implementations ()
  (let ((impls
         '(("sbcl"  sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)
           ("clisp" clisp ("clisp" "-q"))
           ("lisp"  cmucl ("cmucl" "-quiet") :coding-system iso-latin-1-unix)
           ("ccl"   ccl ("ccl")))))
    (cl-loop for (name . args) in impls
       when (eval `(nvp-program ,name :default nil))
       collect args)))

(defun nvp-lisp-start-slime ()
  (interactive)
  (if (slime-connected-p)
      (if (< (length slime-net-processes) 2)
          (slime)
        (slime-list-connections))
    (slime)))

;; -------------------------------------------------------------------
;;; REPL

(with-eval-after-load 'nvp-repl
  (nvp-repl-add '(lisp-mode)
    :modes '(slime-repl-mode)
    :live #'(lambda (p)
              (and (process-buffer p)
                   (let ((buff (slime-connection-output-buffer p)))
                     (buffer-live-p buff))))
    ;; :find-fn #'slime-connection
    :buff->proc #'slime-connection
    :proc->buff #'(lambda (_p) (slime-output-buffer t))
    :init #'(lambda ()
              (save-window-excursion
                (with-current-buffer (slime-output-buffer t)
                  slime-buffer-connection)))))

;; evaluate buffer - if in .asd file, call asdf:load-system on it,
;; otherwise do regular slime eval-buffer
(defun nvp-lisp-eval-buffer (&optional buffer-name)
  (interactive (list (buffer-name)))
  (if (not (string-match-p
            "asd" (file-name-extension (or buffer-name (buffer-name)))))
      (call-interactively #'slime-eval-buffer)
    (let ((system (file-name-sans-extension buffer-name)))
      (slime-repl-eval-string (format "(asdf:load-system \"%s\")" system)))))

;; -------------------------------------------------------------------
;;; Help

;; From http://bc.tech.coop/blog/070515.html
(defun nvp-lisp-lispdoc ()
  "Searches lispdoc.com for SYMBOL, which is by default the symbol 
currently under the curser."
  (interactive)
  (let* ((word-at-point (word-at-point))
	 (symbol-at-point (symbol-at-point))
	 (default (symbol-name symbol-at-point))
	 (inp (read-from-minibuffer
	       (if (or word-at-point symbol-at-point)
		   (concat "Symbol (default " default "): ")
		 "Symbol (no default): "))))
    (if (and (string= inp "")
             (not word-at-point)
             (not symbol-at-point))
	(message "you didn't enter a symbol!")
      (let ((search-type (read-from-minibuffer
			  "full-text (f) or basic (b) search (default b)? ")))
	(browse-url (concat "http://lispdoc.com?q="
			    (if (string= inp "") default inp)
			    "&search="
			    (if (string-equal search-type "f")
				"full+text+search"
			      "basic+search")))))))

;; -------------------------------------------------------------------
;;; Hippie Expand

(defvar nvp-lisp-he-expand-functions
  '(nvp-try-expand-dabbrev-closest-first
    nvp-try-expand-local-abbrevs
    nvp-try-expand-flex
    yas-hippie-try-expand
    try-expand-dabbrev-all-buffers
    try-expand-dabbrev-from-kill
    try-expand-slime-fuzzy
    try-complete-file-name-partially
    try-complete-file-name))

;; pass to `nvp-he-try-expand-history-trans'
(defun nvp-lisp-hippie-expand-setup (&optional repl)
  (setq-local hippie-expand-try-functions-list nvp-lisp-he-expand-functions)
  (setq-local hippie-expand-only-buffers '(lisp-mode))
  (when repl
    (nvp-he-history-setup :history 'slime-repl-input-history
                          :bol-fn #'line-beginning-position
                          :expand-fn #'nvp-he-history-remove-trailing-paren)))

(provide 'nvp-lisp)
;;; nvp-lisp.el ends here
