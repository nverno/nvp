;;; nvp-browse.el --- web browsing -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (defvar nvp-webjump-sites)
  (defvar webjump-sites))
(require 'nvp)
(nvp-decl thing-at-point-url-at-point webjump-builtin webjump-url-fix web-mode)

;;;###autoload
(defun nvp-browse-start ()
  (interactive)
  (call-process browse-url-browser-function nil 0 nil))

;;;###autoload
(defun nvp-browse-url-contents ()
  "Open a new buffer containing the contents of a URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
	 (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    (cond ((search-forward "<?xml" nil t) (xml-mode))
	  ((search-forward "<html" nil t) (web-mode)))))

;; -------------------------------------------------------------------
;;; Webjump

;;;###autoload
(defun nvp-browse-webjump (&optional arg)
  "Jump to website."
  (interactive "P")
  (require 'webjump)
  (require 'nvp-vars)                   ;nvp-webjump-sites
  (let* ((completion-ignore-case t)
         (locals (or (and arg (read-from-minibuffer "URI: "))
                     (and (bound-and-true-p nvp-local-uris)
                          (y-or-n-p "Use local uris?")
                          nvp-local-uris)))
         (sites (or locals (append nvp-webjump-sites webjump-sites)))
         (item (or (and arg (cons nil locals))
                   (assoc-string
                    (nvp-completing-read "WebJump to site: "
                                         (mapcar #'car sites) nil t)
                    sites t)))
         (name (car item))
         (expr (cdr item)))
    (browse-url (webjump-url-fix
                 (cond ((not expr) "")
                       ((stringp expr) expr)
                       ((vectorp expr) (webjump-builtin expr name))
                       ((listp expr) (eval expr))
                       ((symbolp expr)
                        (if (fboundp expr)
                            (funcall expr name)
                          (error "WebJump URL function \"%s\" undefined"
                                 expr)))
                       (t (error "WebJump URL expression for \"%s\" invalid"
                                 name)))))))

(provide 'nvp-browse)
;;; nvp-browse.el ends here
