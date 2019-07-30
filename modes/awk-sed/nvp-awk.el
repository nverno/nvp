;;; nvp-awk.el --- awk extensions -*- lexical-binding: t; -*-

;;; Commentary:
;; - Completion-at-point => merges builtins w/ completion from info manual
;; - when completion finishes and function arguments are available, expans w/
;;   yasnippet
;; - eldoc
;;

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-font))
(require 'nvp)
(require 'info-look)

;; -------------------------------------------------------------------
;;; Syntax

(modify-syntax-entry ?@ "'" (syntax-table))

;; -------------------------------------------------------------------
;;; Font-locking

(nvp-font-lock-add-defaults 'awk-mode
  ("\$\\([0-9]+\\)" (1 font-lock-variable-name-face prepend))
  (`,(concat "\\<" (regexp-opt '("BEGINFILE" "ENDFILE") t) "\\>")
   (1 font-lock-keyword-face))
  ("\\(@[[:alpha:]_]+\\)" (1 'nvp-italic-variable-face)))

;; -------------------------------------------------------------------
;;; Completion
;; PROCINFO['identifiers'], FUNCTAB

(defvar nvp-awk-builtins
  (eval-when-compile
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name
        "doc/builtins.el" (file-name-directory (nvp-load-file-name))))
      (car (read-from-string (buffer-string))))))

(defun nvp-awk-command-completion ()
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

(defun nvp-awk-eldoc--string (cmd)
  (or (gethash cmd nvp-awk-eldoc-cache)
      (-when-let (plist (cdr (assoc-string cmd nvp-awk-builtins)))
        (let ((pars (plist-get plist :param)))
          (setf (gethash cmd nvp-awk-eldoc-cache)
                (format "%s: %s" (propertize cmd 'face 'font-lock-function-name-face)
                        (if pars (concat "(" (mapconcat 'identity pars ", ") ")")
                          (plist-get plist :desc))))))))

(defun nvp-awk-eldoc-function ()
  (-when-let (sym (thing-at-point 'symbol))
    (nvp-awk-eldoc--string sym)))

(provide 'nvp-awk)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-awk.el ends here
