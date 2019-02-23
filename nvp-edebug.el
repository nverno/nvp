;;; nvp-edebug.el --- edebugging helper -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-22 23:18:17>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 20 February 2019

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'edebug)
(nvp-declare "" nvp-help-describe-keymap)

;; setup eval with elisp minibuffer eval hooks
;;;###autoload
(defun nvp-edebug-eval-expression (expr)
  (interactive (list (read--expression "Eval: ")))
  (edebug-eval-expression expr))

;;;###autoload
(defun nvp-edebug-help ()
  (interactive)
  (nvp-help-describe-keymap 'edebug-mode-map))

(provide 'nvp-edebug)
;;; nvp-edebug.el ends here
