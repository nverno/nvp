;;; nvp-browse.el --- web browsing -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-22 18:24:56>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created:  2 December 2016

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (defvar webjump-sites))
(require 'nvp)
(declare-function thing-at-point-url-at-point "thingatpt")
(nvp-declare "webjump" webjump-builtin webjump-url-fix)
(declare-function web-mode "web-mode")

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
