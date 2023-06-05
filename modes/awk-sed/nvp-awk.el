;;; nvp-awk.el --- awk extensions -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; - Completion-at-point => merges builtins w/ completion from info manual
;; - when completion finishes and function arguments are available, expans w/
;;   yasnippet
;; - eldoc
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'cc-awk)
(require 'nvp)
(require 'info-look)
(nvp:decls)

;;; Syntax

(modify-syntax-entry ?@ "'" awk-mode-syntax-table)
(modify-syntax-entry ?$ "'" awk-mode-syntax-table)

;;; Font-locking

(nvp:font-lock-add-defaults 'awk-mode
  ("\$\\([0-9]+\\)" (1 font-lock-variable-name-face prepend))
  ((concat "\\<" (regexp-opt '("BEGINFILE" "ENDFILE") t) "\\>")
   (1 font-lock-keyword-face))
  ("\\(@[[:alpha:]_]+\\)" (1 'nvp-italic-variable-face)))

(defconst nvp-awk-imenu-expression
  `((nil
     ,(concat "^\\s-*" (regexp-opt '("BEGIN" "BEGINFILE" "END" "ENDFILE") 'paren))
     0)
    (nil "^\\s-*function\\s-+\\([[:alpha:]][[:alpha:]]+\\)\\s-*(" 1)))

;; -------------------------------------------------------------------
;;; Completion

;; PROCINFO['identifiers'], FUNCTAB

(defconst nvp-awk-builtins
  (eval-when-compile
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name
        "doc/builtins.el" (file-name-directory (nvp:load-file-name))))
      (car (read-from-string (buffer-string))))))

(defun nvp-awk-completion-at-point ()
  (-when-let ((beg . end) (bounds-of-thing-at-point 'symbol))
    (list beg end (completion-table-merge
                   nvp-awk-builtins
                   (info-lookup->completions 'symbol 'awk-mode))
          :exclusive 'no
          :company-docsig
          (lambda (s) (plist-get (cdr (assoc-string s nvp-awk-builtins)) :desc))
          :company-doc-buffer
          (lambda (s) (company-doc-buffer
                  (plist-get (cdr (assoc-string s nvp-awk-builtins)) :desc)))
          :exit-function
          (lambda (str status)
            (when (eq 'finished status)
              (-when-let* ((plist (cdr (assoc str nvp-awk-builtins)))
                           (params (plist-get plist :param)))
                (nvp-awk--post-complete-snippet params)))))))

;;; Snippets

;; expand function arguments after completion
(defun nvp-awk--post-complete-snippet (params)
  (yas-expand-snippet 
   (concat
    "(" (mapconcat
         'identity (--map-indexed (format "${%d:%s}" (1+ it-index) it) params)
         ", ") ")")))

;;; Eldoc

(defvar nvp-awk-eldoc-cache (make-hash-table :test 'equal))

(defun nvp-awk-current-command ()
  (save-excursion
    (let ((lbp (line-beginning-position))
          open)
      (while (or (and (setq open (nth 8 (syntax-ppss)))
                      (goto-char open))
                 (condition-case nil
                     (and (eq (char-before) ?\))
                          (forward-sexp -2)
                          t)
                   (scan-error nil))
                 (/= 0 (skip-chars-backward "^({;=" lbp))))
      (when (eq (char-before) ?\()
        (forward-char -1)
        (skip-syntax-backward " " lbp)
        (skip-chars-backward "[A-Za-z0-9_]" lbp))
      (and (looking-at "\\s-*\\([A-Za-z][A-Za-z0-9_]*\\)")
           (match-string-no-properties 1)))))

(defun nvp-awk-eldoc--string (cmd)
  (or (gethash cmd nvp-awk-eldoc-cache)
      (-let (((&plist :param pars :desc desc)
              (cdr (assoc-string cmd nvp-awk-builtins))))
        (when (or pars desc)
          (setf (gethash cmd nvp-awk-eldoc-cache)
                (format "%s: %s"
                        (propertize cmd 'face 'font-lock-function-name-face)
                        (if pars (concat "(" (mapconcat 'identity pars ", ") ")")
                          desc)))))))

(defun nvp-awk-eldoc-function ()
  (--when-let (nvp-awk-current-command)
    (nvp-awk-eldoc--string it)))


(declare-function info-lookup-add-help "info-look")
(with-eval-after-load 'info-look
  ;; overwrite `awk-mode' defaults: #<marker at 36466 in info-look.el.gz>
  ;; TODO: submit patch
  (info-lookup-add-help
   :mode 'awk-mode
   :regexp "[_a-zA-Z]+"
   :doc-spec '(("(gawk)Index"
	        (lambda (item)
		  (let ((case-fold-search nil))
		    (cond
                     ;; BEG, END, RSTART, etc.
                     ((string-match "^\\([A-Z]+\\) \\(?:variable\\|pattern\\)" item)
                      (match-string 1 item))
                     ;; XXX(6/5/23): these dont match anything
                     ;; `BEGIN' and `END'.
                     ((string-match "^\\([A-Z]+\\) special pattern\\b" item)
                      (match-string 1 item))
		     ;; `if', `while', `do', ...
		     ((string-match "^\\([a-z]+\\) statement\\b" item)
		      (if (not (string-equal (match-string 1 item) "control"))
			  (match-string 1 item)))
		     ;; `NR', `NF', ...
		     ((string-match "^[A-Z]+$" item)
		      item)
		     ;; Built-in functions (matches to many entries).
		     ((string-match "^[a-z]+$" item)
		      item))))
	        "['`‘]" "\\([ \t]*([^)]*)\\)?['’]"))))

(provide 'nvp-awk)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-awk.el ends here
