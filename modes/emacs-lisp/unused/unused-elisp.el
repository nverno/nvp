
;;; FIXME: update or remove
;; convert selected bindings to macro form and align
;; (defun nvp-macroify-bindings (start end)
;;   (interactive "r")
;;   (goto-char start)
;;   (let ((map (save-excursion
;;                (when (re-search-forward "\\([a-zA-Z0-9-]+\\)-map"
;;                                         end t)
;;                  (match-string-no-properties 1)))))
;;     (when map
;;       (let (binds)
;;         (while (re-search-forward
;;                 "\\(\"[^\"]+\"\\))?[\n\t ]*[#']*\\([a-zA-Z0-9-]+\\)"
;;                 end t)
;;           (push (format "(%s . %s)"
;;                         (match-string-no-properties 1)
;;                         (match-string-no-properties 2))
;;                 binds))
;;         (goto-char start)
;;         (insert (concat "(nvp:bindings \"" map "\" nil \n  "
;;                         (mapconcat 'identity (nreverse binds) "\n  ")
;;                         ")\n"))
;;         (goto-char start)
;;         (mark-sexp)
;;         (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)\\. ")))))
