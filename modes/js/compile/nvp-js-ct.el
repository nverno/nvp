;;; nvp-js-ct.el --- compile-time -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp-decls)

;; -------------------------------------------------------------------
;;; Snippet helpers
;;; XXX: these are all probably useless -- just use js2 parser

(defun nvp-js-method-p ()
  (save-excursion
    (word-search-backward "function")
    (looking-back ": " (line-beginning-position))))

(defun nvp-js-function-declaration-p ()
  (save-excursion
    (word-search-backward "function")
    (looking-back "^\\s *" (line-beginning-position))))

(defun nvp-js-snippet-punctuation ()
  (if (nvp-js-method-p)
      (when (not (looking-at "[ \n\t\r]*[},]"))
        (insert ","))
    (unless (nvp-js-function-declaration-p)
      (if (looking-at "$") (insert ";")))))

(defun nvp-js-snippet-function-name ()
  (if (nvp-js-function-declaration-p) "name" ""))

(provide 'nvp-js-ct)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-js-ct.el ends here
