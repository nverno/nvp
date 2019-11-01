;;; nvp-hippie-history.el --- hippie expand from histories -*- lexical-binding: t; -*-

;;; Commentary:

;; Hippie expansion using histories, eg. shell history rings, minibuffer
;; history, etc.

;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'hippie-exp)
(declare-function eshell-beginning-of-input "esh-mode")
(declare-function comint-line-beginning-position "comint")

;; -------------------------------------------------------------------
;;; Variables to control getting / expanding mode history

;; function to return point at beginning of input line
(defvar-local nvp-he-history-bol-fn nil
  "Function to return beginning position for current candidate.")

;; history candidates, eg eshell-history-ring, comint-input-ring
(defvar-local nvp-he-history nil)

(defvar-local nvp-he-history-fn #'nvp-he-history-default-fn
  "Function to retrieve history candidates.")

;; after expanding candidate, call this function on the expansion.
;; e.g for expanding in a lisp shell, remove a trailing ')'
(defvar-local nvp-he-history-post-expand-fn #'identity
  "Function called on substitution candidate prior to insertion.")

;; -------------------------------------------------------------------
;;; Defaults

;; mapping for shells to history rings
(defvar nvp-he-history-alist
  '((shell-mode               . comint-input-ring)
    (eshell-mode              . eshell-history-ring)
    (minibuffer-inactive-mode . minibuffer-history-variable)
    ;; default - for any mode that is comint derived
    (comint-mode              . comint-input-ring))
  "Mapping of modes to history rings.")

;; for eshell
(defsubst nvp-he-history-eshell-bol ()
  (marker-position (eshell-beginning-of-input)))

(defvar nvp-he-history-bol-alist
  '((shell-mode               . comint-line-beginning-position)
    (eshell-mode              . nvp-he-history-eshell-bol)
    (minibuffer-inactive-mode . line-beginning-position)
    (comint-mode              . comint-line-beginning-position))
  "Mapping of modes to functions returning line beginning position.")

;; default function to retrieve history elements for prefix
(defun nvp-he-history-default-fn (prefix history)
  ;; eg. minibuffer-history-variable -> read-expression-history -> contents
  (while (and history (symbolp history) (boundp history))
    (setq history (symbol-value history)))
  (cl-remove-duplicates
   (all-completions prefix (if (ring-p history) (ring-elements history) history))
   :test #'string= :from-end t))

;; -------------------------------------------------------------------
;;; Post expansion transforms

;; remove trailing ')' if first char of STR is '(', for expanding in lispy shells
(defun nvp-he-history-remove-trailing-paren (str)
  (and str
       (if (and (string= "(" (substring str 0 1))
                (string= ")" (substring str -1)))
           (substring str 0 -1)
         str)))

;; Hippie expansion from history, eg. shell (comint) history rings or minibuffer
;; OLD must be nil on first call to function, and t for
;; successive expansions . 
;;;###autoload
(defun nvp-he-try-expand-history (old)
  (unless old
    (he-init-string (funcall nvp-he-history-bol-fn) (point))
    (unless (he-string-member he-search-string he-tried-table)
      (setq he-tried-table (cons he-search-string he-tried-table)))
    (setq he-expand-list                ;build expansion list
          (and (not (equal "" he-search-string))
               (funcall nvp-he-history-fn he-search-string nvp-he-history))))
  (while (and he-expand-list            ;remove seen candidates
              (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (prog1 (not (null he-expand-list))
    (if (not he-expand-list)
        (and old (he-reset-string))
      (he-substitute-string
       (funcall nvp-he-history-post-expand-fn (pop he-expand-list)) 'trans-case))))

;; -------------------------------------------------------------------
;;; Setup 

;;;###autoload
(cl-defun nvp-he-history-setup (&key history bol-fn history-fn expand-fn)
  "Setup shell to use hippie expansion for shell history."
  (interactive)
  (let* ((comint-p (derived-mode-p 'comint-mode))
         (history (or history
                      (cdr (assoc major-mode nvp-he-history-alist))
                      (and comint-p
                           (cdr (assoc 'comint-mode nvp-he-history-alist)))))
         (bol (or bol-fn
                  (cdr (assoc major-mode nvp-he-history-bol-alist))
                  (and comint-p
                       (cdr (assoc 'comint-mode nvp-he-history-bol-alist))))))
    (when (and (or (functionp bol) (fboundp bol))
               (or history-fn (not (null history))))
      (setq nvp-he-history history)
      (setq nvp-he-history-bol-fn bol)
      (and expand-fn (setq nvp-he-history-post-expand-fn expand-fn))
      (and history-fn (setq nvp-he-history-fn history-fn))
      (unless (local-variable-p 'hippie-expand-try-functions-list)
        (make-local-variable 'hippie-expand-try-functions-list))
      (add-to-list 'hippie-expand-try-functions-list 'nvp-he-try-expand-history))))

(provide 'nvp-hippie-history)
;;; nvp-hippie-history.el ends here
