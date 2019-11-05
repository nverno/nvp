;;; cmakecache-mode.el --- major mode for CMakeCache.txt -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created:  5 November 2019

;;; Commentary:
;;; Code:

(defconst cmakecache-mode-font-lock-keywords
  `(("^\\([[:alnum:]_-]+\\):\\([[:alnum:]]+\\)="
     (1 'font-lock-variable-name-face)
     (2 'font-lock-type-face))
    (,(regexp-opt '("ON" "OFF") t) . font-lock-constant-face)))

(defvar cmakecache-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 12b" st)
    (modify-syntax-entry ?\# "< b" st)
    (modify-syntax-entry ?\n "> b" st)
    st))

;;;###autoload
(define-derived-mode cmakecache-mode text-mode "CMCache"
  "Major mode for CMakeCache.txt files."
  :abbrev-table nil
  (setq font-lock-defaults '(cmakecache-mode-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("CMakeCache.txt\\'" . cmakecache-mode))

(provide 'cmakecache-mode)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; cmakecache-mode.el ends here
