;;; nvp-webjump.el --- webjumps -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-cache))
(require 'nvp)
(require 'nvp-vars)
(require 'webjump)
(nvp:decls)

(defvar nvp-webjump-org-links-re (regexp-opt '("reference" "links"))
  "Org sections to look for links.")

(defvar nvp-webjump-cache nil "Cache local uris from files.")

(defvar nvp-webjump-sites)              ;nvp-vars

(defun nvp-webjump-wikipedia (_site)
  (let ((thing (thing-at-point 'symbol 'noprops)))
    (when (null thing)
      (setq thing (read-string "Wikipedia lookup: ")))
    (format "https://wikipedia.org/wiki/%s" thing)))

(defun nvp-webjump-options (base options &optional prompt sep)
  (concat (string-trim-right base "/*") (or sep "/")
          (nvp-completing-read (or prompt "ext: ") options)))

(defun nvp-webjump-multiple (prompt alist)
  (let* ((choice (nvp-completing-read prompt alist nil t))
         (urls (assoc-string choice alist 'case-fold)))
    (cons (cadr urls) (mapconcat 'identity (cddr urls) " "))))

(defun nvp-get-local-uris ()
  "Find local jump uris. Use `nvp-local-uris' or if a local notes file is found,
try to find links there."
  (or (bound-and-true-p nvp-local-uris)
      (--when-let (nvp:find-notes-file)
        (unless nvp-webjump-cache
          (setq nvp-webjump-cache (nvp-cache :expires-fn 'modtime)))
        (nvp:cache-get nvp-local-notes-file nvp-webjump-cache
          (nvp-org-links nvp-webjump-org-links-re it)))))

;;;###autoload
(defun nvp-browse-webjump (&optional prompt use-defaults)
  "Jump to website."
  (interactive (list (nvp:prefix 4) (nvp:prefix 16)))
  (-let* (browse-url-chrome-arguments
          (completion-ignore-case t)
          (locals (and (not use-defaults)
                       (or (and prompt (read-from-minibuffer "URI: "))
                           (nvp-get-local-uris))))
          (sites (or locals (append nvp-webjump-sites webjump-sites)))
          ((name . expr)
           (or (and prompt (cons nil locals))
               (assoc-string
                (nvp-completing-read "WebJump to site: " sites nil t)
                sites 'case-fold)))
          (url
           (cond ((not expr) "")
                 ((stringp expr) expr)
                 ((vectorp expr) (webjump-builtin expr name))
                 ((listp expr)
                  (if (eq (car expr) 'multiple)
                      (-let (((url . args) (apply #'nvp-webjump-multiple (cdr expr))))
                        (push args browse-url-chrome-arguments)
                        url)
                    (eval expr)))
                 ((symbolp expr)
                  (if (fboundp expr) (funcall expr name)
                    (error "WebJump URL function \"%s\" undefined" expr)))
                 (t (error "WebJump URL expression for \"%s\" invalid" name)))))
    (browse-url (webjump-url-fix url))))

(provide 'nvp-webjump)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-webjump.el ends here
