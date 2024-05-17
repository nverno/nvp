;;; hippie-expand-nrepl.el --- hippie expand nrepl history -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'hippie-exp)
(require 'cider-repl nil t)
(nvp:decls :v (cider-repl-input-start-mark cider-repl-input-history))

(defvar-local he-nrepl--index nil)
(defvar-local he-nrepl--matches ())

;; return point at beginning of prompt
(defsubst he-nrepl-bol ()
  (marker-position cider-repl-input-start-mark))

;; if expansion starts with '(', remove trailing ')', paredit will do rest
(defsubst he-repl-expand (str)
  (and str
       (if (and (string= "(" (substring str 0 1))
                (string= ")" (substring str -1)))
           (substring str 0 -1)
         str)))

;; Hippie expansion from nrepl history
;; OLD must be nil on first call to function, and t for  successive expansions.
;;;###autoload
(defun try-expand-nrepl-history (old)
  (and cider-repl-input-history
       (let (expansion)
         (unless old
           (he-init-string (he-nrepl-bol) (point))
           (setq he-nrepl--index 0)
           (setq he-nrepl--matches
                 (and (not (equal "" he-search-string))
                      (cl-remove-duplicates
                       (all-completions
                        he-search-string
                        cider-repl-input-history)
                       :test 'string=
                       :from-end t))))
         (when he-nrepl--matches
           (setq expansion (he-repl-expand
                            (nth he-nrepl--index he-nrepl--matches)))
           (setq he-nrepl--index (1+ he-nrepl--index)))
         (if (not expansion)
             (ignore (and old (he-reset-string)))
           (he-substitute-string expansion t)))))

;;;###autoload
(defun hippie-expand-nrepl-setup ()
  (interactive)
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-expand-nrepl-history))

(provide 'hippie-expand-nrepl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; hippie-expand-nrepl.el ends here
