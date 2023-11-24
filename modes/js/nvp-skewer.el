;;; nvp-skewer.el --- skewer-mode util -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-repl)
(nvp:decls :p (skewer web html css) :f (list-skewer-clients run-skewer)
           :v (nvp-js-modes))

(when (fboundp 'skewer-repl)
  (nvp-repl-add (append '(web-mode html-mode css-mode css-ts-mode) nvp-js-modes)
    :name 'skewer
    :modes '(skewer-repl-mode)
    :bufname (regexp-quote "*skewer-repl*")
    :init (lambda (&optional prefix)
            (save-window-excursion
              (skewer-repl)
              (when (seq-empty-p skewer-clients)
                (run-skewer prefix))
              (get-buffer-process (current-buffer))))
    :send-buffer #'skewer-load-buffer
    ;; FIXME: these use `js2-mode' nodes
    :send-defun #'skewer-eval-defun
    :send-sexp #'skewer-eval-last-expression
    :history-file ".skewer_history"
    :help-cmd (lambda (&rest _) (list-skewer-clients))))

(defun nvp-skewer-eval-last-expression (&optional print)
  (interactive "P")
  (call-interactively 
   (if print #'skewer-eval-print-last-expression
     #'skewer-eval-last-expression)))

(provide 'nvp-skewer)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-skewer.el ends here
