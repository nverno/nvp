;;; company-utop ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'utop-inf)
(require 'company)

(defun company-utop--candidates (arg)
  (setq utop-inf-completions nil)
  (utop-inf-redirect arg)
  utop-inf-completions)

(defun company-utop--prefix ()
  (and (eq major-mode 'utop-mode)
       (buffer-substring utop-prompt-max (point))))

;; (defun company-utop--annotation (candidate)
;;   (or (get-text-property 0 'annot candidate) ""))

;; (defun company-utop--meta (candidate)
;;   (get-text-property 0 'meta candidate))

;; (defun company-utop--doc (candidate)
;;   (company-doc-buffer ""))

;; (defun company-utop--location (candidate))

;;;###autoload
(defun company-utop (command &optional arg &rest _args)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-utop))
    (prefix (company-utop--prefix))
    ;; (annotation (company-utop--annotation arg))
    ;; (doc-buffer (company-utop--doc arg))
    ;; (location (company-utop--location arg))
    (candidates (company-utop--candidates arg))
    (require-match 'never)
    (duplicates nil)
    (sorted t)))

(provide 'company-utop)
;;; company-utop.el ends here
