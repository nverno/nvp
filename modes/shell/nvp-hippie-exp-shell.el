;;; nvp-hippie-exp-shell.el --- expand shell aliases -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/shell-tools
;; Last modified: <2019-03-31 00:58:29>
;; Created:  7 December 2016

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'hippie-exp)
(require 'nvp-shell)

;; -------------------------------------------------------------------
;;; Expand shell aliases, eg. bash shell-expand-alias C-M-e 

(defvar-local nvp-he-shell-alias-beg ()
  "Returns beginning position of previous shell alias.")
(setq-default nvp-he-shell-alias-beg
              #'(lambda () (car (bounds-of-thing-at-point 'symbol))))

;;;###autoload
(defun nvp-he-try-expand-shell-alias (old)
  "Expand shell alias, like bash shell-expand-alias."
  (cl-block nil
    (unless old
      (let ((beg (funcall nvp-he-shell-alias-beg)))
        (and (not beg) (cl-return))
        (he-init-string beg (point))
        (unless (he-string-member he-search-string he-tried-table)
          (setq he-tried-table (cons he-search-string he-tried-table)))
        (setq he-expand-list             ;completions from hash table
              (and (not (equal "" he-search-string))
                   (delq nil
                         (mapcar
                          (lambda (abbr)
                            (gethash abbr (nvp-shell-alias-table) nil))
                          (all-completions
                           he-search-string (nvp-shell-alias-table))))))))
    (while (and he-expand-list         ;remove seen strings from table
                (he-string-member (car he-expand-list) he-tried-table t))
      (setq he-expand-list (cdr he-expand-list)))
    (prog1 (not (null he-expand-list))
      (if (null he-expand-list)
          (and old (he-reset-string))
        (he-substitute-string (pop he-expand-list) t)))))

(provide 'nvp-hippie-exp-shell)
;;; nvp-hippie-exp-shell.el ends here
