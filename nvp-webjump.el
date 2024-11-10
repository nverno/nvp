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
(nvp:decls :f (org-link-open nvp-org-links))

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
          (completing-read (or prompt "ext: ") options)))

(defun nvp-webjump-multiple (prompt alist)
  (let* ((choice (completing-read prompt alist nil t))
         (urls (assoc-string choice alist 'case-fold)))
    (cons (cadr urls) (mapconcat 'identity (cddr urls) " "))))

(defun nvp-get-local-uris ()
  "Find local jump uris.
Use `nvp-local-uris' or if a local notes file is found, try to find links
there."
  (or (bound-and-true-p nvp-local-uris)
      (--when-let (nvp:find-notes-file)
        (unless nvp-webjump-cache
          (setq nvp-webjump-cache (nvp-cache :expires-fn 'modtime)))
        (nvp:cache-get nvp-local-notes-file nvp-webjump-cache
          (nvp-org-links nvp-webjump-org-links-re it)))))

;;;###autoload
(defun nvp-browse-webjump (&optional prompt use-defaults choices no-browse)
  "Jump to website."
  (interactive (let ((raw (prefix-numeric-value current-prefix-arg)))
                 (list (= raw 4) (>= raw 16))))
  (-let* ((extra-args nil)
          (type nil)
          (completion-ignore-case t)
          (locals (and (not (or choices use-defaults))
                       (or (and prompt (read-from-minibuffer "URI: "))
                           (nvp-get-local-uris))))
          (sites (or choices locals (append nvp-webjump-sites webjump-sites)))
          ((name . expr)
           (or (and prompt (cons nil locals))
               (assoc-string
                (completing-read "WebJump to site: " sites nil t)
                sites 'case-fold)))
          (url
           (cond ((not expr) "")
                 ((stringp expr) expr)
                 ((vectorp expr) (webjump-builtin expr name))
                 ((listp expr)
                  (pcase (car expr)
                    ('choice
                     (funcall #'nvp-browse-webjump nil nil
                              (cdr expr) 'no-browse))
                    ('multiple
                     (-let (((url . args)
                             (funcall #'nvp-webjump-multiple
                                      (cadr expr) (cddr expr))))
                       (setq type 'multiple)
                       (push args extra-args)
                       url))
                    ('org-link
                     (setq type 'org-link)
                     (cdr expr))
                    (_ (eval expr))))
                 ((symbolp expr)
                  (if (fboundp expr) (funcall expr name)
                    (error "WebJump URL function \"%s\" undefined" expr)))
                 (t (error "WebJump URL expression for \"%s\" invalid" name)))))
    (if no-browse
        (webjump-url-fix url)
      (pcase type
        ;; TODO(7/11/24): check args to open multiple in firefox
        ('multiple (let ((browse-url-chrome-arguments extra-args)
                         (browse-url-browser-function #'browse-url-chrome))
                     (browse-url (webjump-url-fix url))))
        ('org-link (apply #'org-link-open url))
        (_ (browse-url (webjump-url-fix url)))))))

(provide 'nvp-webjump)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-webjump.el ends here
