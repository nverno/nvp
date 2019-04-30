;;; nvp-he-lisp.el --- lisp hippie expansion -*- lexical-binding: t; -*-

;;; Commentary:

;; Fuzzy expansion for hyphen-separated commands, eg. i-p => in-package

;; TODO:
;; - generalize flex expansion to use arbitrary regex / cache

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'hippie-exp)

;; collect matches for REGEXP. If BUFFER is non-nil, collect matches from
;; BUFFER (default current buffer)
(nvp-define-cache nvp-he-lisp-matches (regexp &optional buffer)
  "Collect matches in buffer, invalidating when candidates are exhausted."
  :local t
  (and buffer (set-buffer buffer))
  (let (res)
   (save-excursion
     (goto-char (point-min))
     (while (re-search-forward regexp nil 'move)
       (push (match-string-no-properties 0) res)))
   (cl-remove-duplicates res :test #'equal)))

;; create regexp from STR matching expansions around hypens, eg
;; r-r => "\\br\\w*-r[A-Za-z0-9-]*\\b"
;; so it matches replace-regexp-in-string, for example
(defsubst nvp-he-lisp-regexpify (str)
  (concat "\\b" (replace-regexp-in-string "-" "\\\\w*-" str) "[A-Za-z0-9-]*\\b"))

;;;###autoload
(defun nvp-he-try-expand-flex-lisp (old)
  "Try to complete lisp symbol from current buffer, using fuzzy matching around
'-' separators, eg. \"i-p\" => \"in-package\"."
  (unless old
    ;; possibly, slime-symbol-start-pos, slime-symbol-end-pos
    (he-init-string (he-lisp-symbol-beg) (point))
    (unless (he-string-member he-search-string he-tried-table)
      (setq he-tried-table (cons he-search-string he-tried-table)))
    (setq he-expand-list                ;build expansion list
          (and (not (equal "" he-search-string))
               (nvp-he-lisp-matches (nvp-he-lisp-regexpify he-search-string)))))
  (while (and he-expand-list            ;remove candidates already found
              (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (prog1 (not (null he-expand-list))    ;return t if expansion is found
    (if he-expand-list (he-substitute-string (pop he-expand-list))
      (and old (he-reset-string))
      (setq nvp-he-lisp-matches nil)))) ;invalidate cache

(provide 'nvp-he-lisp)
;;; nvp-he-lisp.el ends here
