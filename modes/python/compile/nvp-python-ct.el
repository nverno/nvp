;;; nvp-python-ct.el --- compile time include -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'python)

;;; Encoding
;; from prelude to guess encoding
(defsubst nvp-python-encoding-comment-required-p ()
  (re-search-forward "[^\0-\177]" nil t))

(defsubst nvp-python-detect-encoding ()
  (let ((coding-system (or save-buffer-coding-system
	                   buffer-file-coding-system)))
    (if coding-system
	(symbol-name
	 (or (coding-system-get coding-system 'mime-charset)
	     (coding-system-change-eol-conversion coding-system nil)))
      "ascii-8bit")))

(defsubst nvp-python-insert-coding-comment (encoding)
  (let ((newlines (if (looking-at "^\\s *$") "\n" "\n\n")))
    (insert (format "# coding: %s" encoding) newlines)))

;; bounds of current python statement
(defsubst nvp-python-statement-bounds ()
  (cons (python-nav-beginning-of-statement) (python-nav-end-of-statement)))

(provide 'nvp-python-ct)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-python-ct.el ends here
