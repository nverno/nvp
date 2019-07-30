;;; nvp-mips.el --- MIPs helpers/completion -*- lexical-binding: t; -*-
;;; Commentary:
;; Note: install Mars or spim
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (defvar mips-font-lock-keywords)
  (defvar mips-font-lock-directives)
  (defvar mips-font-lock-pseudoinstructions)
  (defvar mips-font-lock-deprecated)
  (defvar company-backends))
(require 'nvp)
(require 'company)
(require 'mips-mode nil t)

(nvp-package-define-root :snippets t)

(defvar nvp-mips-header-re "^[a-zA-Z0-9_.]+:")

;; -------------------------------------------------------------------
;;; Help

;;;###autoload
(defun nvp-mips-help ()
  (interactive)
  (browse-url "http://www.cburch.com/cs/330/reading/mips-ref.pdf"))

;; -------------------------------------------------------------------
;;; Commands

(defun nvp-mips-previous-label ()
  "Goto previous label"
  (interactive)
  (condition-case nil
      (progn
        (forward-line -1)
        (re-search-backward "^[ \t]*[a-zA-Z0-9_]+:")
        (beginning-of-line))
    (error
     (forward-line 1)
     (user-error "No previous label"))))

(defun nvp-mips-next-label ()
  "Goto next label"
  (interactive)
  (condition-case nil
      (progn
        (forward-line 1)
        (re-search-forward "^[ \t]*[a-zA-Z0-9_.]+:")
        (beginning-of-line))
    (error
     (forward-line -1)
     (user-error "No previous label"))))

;; -------------------------------------------------------------------
;;; Completion

(defconst mips-words
  (progn
    (cl-loop for directive in mips-font-lock-directives
       for pseudo in mips-font-lock-pseudoinstructions
       for depr in mips-font-lock-deprecated
       do (add-text-properties 0 1 (list 'annot "<directive>") directive)
         (add-text-properties 0 1 (list 'annot "<pseudo>") pseudo)
         (add-text-properties 0 1 (list 'annot "<deprecated>") depr))
    (append mips-font-lock-directives mips-font-lock-keywords
            mips-font-lock-pseudoinstructions mips-font-lock-deprecated)))

(defun company-mips--annotation (candidate)
  (or (get-text-property 0 'annot candidate) ""))

(defun company-mips (command &optional arg &rest _ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-mips))
    (prefix (and (derived-mode-p 'mips-mode))
            (not (company-in-string-or-comment))
            (or (company-grab-symbol) 'stop))
    (annotation (company-mips--annotation arg))
    (candidates (all-completions arg mips-words))))

;;;###autoload
(defun nvp-mips-completion-setup ()
  (make-local-variable 'company-backends)
  (cl-pushnew 'company-mips company-backends))

(provide 'nvp-mips)
;;; nvp-mips.el ends here
