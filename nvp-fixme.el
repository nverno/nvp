;;; nvp-fixme.el --- simple fixme minor-mode -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-22 20:53:32>
;; Created: 30 November 2016

;;; Commentary:

;; Minor mode to highlight and navigate b/w fixmes and todos

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))

(eval-and-compile (defvar nvp-fixme-keywords "\\<\\(TODO\\|FIXME\\):"))
(nvp-defvar nvp-fixme-font-lock-words
  `((,nvp-fixme-keywords 1 'font-lock-warning-face prepend)))

;; collect occurences of fixme keywords in buffer
(nvp-function-with-cache nvp-fixme-collect-occurences () nil
  :local t
  :predicate (not (buffer-modified-p))
  (save-excursion
    (goto-char (point-min))
    (let (res)
      (while (re-search-forward nvp-fixme-keywords nil 'move)
        ;; only store locations inside of comments
        (and (nth 4 (syntax-ppss))
             (push (line-number-at-pos) res)))
      res)))

;; -------------------------------------------------------------------
;;; Commands 

(defun nvp-fixme-search (&optional back)
  (let ((case-fold-search t)
        (search-fn (if back 're-search-backward 're-search-forward))
        (beg (point)))
    (condition-case nil
        (progn
          (when (looking-at-p nvp-fixme-keywords)
            (forward-line (and back -1)))
          (funcall search-fn nvp-fixme-keywords)
          (goto-char (match-beginning 0)))
      (error (message "No more fixmes") (goto-char beg)))))

(defun nvp-fixme-next ()
  (interactive)
  (nvp-fixme-search))

(defun nvp-fixme-previous ()
  (interactive)
  (nvp-fixme-search 'back))

(defun nvp-fixme-occur ()
  (interactive)
  (occur nvp-fixme-keywords))

;; -------------------------------------------------------------------
;;; Mode 

(defvar nvp-fixme-menu
  '("Fixme"
    ["Next" nvp-fixme-next t]
    ["Previous" nvp-fixme-previous t]
    ["Occur" nvp-fixme-occur t]
    "---"
    ["Turn off" nvp-fixme-mode t]))

(defvar nvp-fixme-mode-map
  (let ((km (make-sparse-keymap)))
    (easy-menu-define nil km nil nvp-fixme-menu)
    (define-key km (kbd "M-s-n") 'nvp-fixme-next)
    (define-key km (kbd "M-s-p") 'nvp-fixme-previous)
    (define-key km (kbd "M-s-o") 'nvp-fixme-occur)
    km))

;;;###autoload
(define-minor-mode nvp-fixme-mode "Fixme"
  nil
  :lighter " Fixme"
  :keymap nvp-fixme-mode-map
  (if nvp-fixme-mode
      (font-lock-add-keywords nil nvp-fixme-font-lock-words)
    (font-lock-remove-keywords nil nvp-fixme-font-lock-words))
  (font-lock-flush)
  (font-lock-ensure))

(provide 'nvp-fixme)
;;; nvp-fixme.el ends here
