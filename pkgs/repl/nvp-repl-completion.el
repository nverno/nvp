;;; nvp-repl-completion.el --- Completion at point for REPL commands -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-repl))

;;;###autoload
(defun nvp-repl--make-completion-at-point ( name commands
                                            &optional cmd-prefix pos-bol)
  (when commands
    (let ((pos-bol (or pos-bol
                       (and (derived-mode-p 'comint-mode)
                            #'comint-line-beginning-position)))
          (fname (format "nvp-repl-%s-completion-at-point" (symbol-name name))))
      (defalias (intern fname)
        (lambda ()
          (let ((end (point))
                (beg (save-excursion
                       (goto-char (funcall pos-bol))
                       (skip-syntax-forward " " (line-end-position))
                       (point))))
            (when (or (null cmd-prefix)
                      (and (eq cmd-prefix (char-after beg))
                           (setq beg (1+ beg))))
              (list beg end
                    (completion-table-with-cache (lambda (_s) commands))
                    :exclusive 'no))))))))

(provide 'nvp-repl-completion)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-repl-completion.el ends here
