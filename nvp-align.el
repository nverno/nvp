;;; nvp-align.el --- alignment -*- lexical-binding: t; -*-

;;; Commentary:

;; Alignment:
;; - main interface using mode rules
;; - TODO: describe rules applicable for mode
;; - additional alignment rules
;; - other interactive functions

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(nvp-decls)
(require 'nvp)
(require 'nvp-read)

;; align with spaces
(define-advice align (:around (old-fn &rest args) "no-tabs")
  (let ((indent-tabs-mode nil))
    (apply old-fn args)))

;;;###autoload
(defun nvp-align (&optional arg beg end)
  "Align buffer region b/w BEG and END, or call `nvp-mark-defun' if nil. 
(4) prefix, align entire active region or buffer. 
(16) prefix, highlight changes that would occur."
  (interactive
   (cons (prefix-numeric-value current-prefix-arg)
         (nvp-tap-or-region 'bdwim (nvp-prefix 4 'buffer 'defun) :pulse t)))
  (if (eq arg 16) (call-interactively 'align-highlight-rule)
    (align beg end)))

(defun nvp-align-mode-rules (&optional mode)
  "Display alignment rules applicable to MODE (default `major-mode').
With prefix, prompt for mode."
  (interactive (list (nvp-prefix 4 (nvp-read-mode) major-mode)))
  (nvp-with-results-buffer (help-buffer)
    ))

(provide 'nvp-align)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-align.el ends here
