;;; nvp-info-capf.el --- info completion-at-point -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'info-look)
(require 'nvp)
(nvp:decls)

;;;###autoload
(defun nvp-info-completion-at-point ()
  (-when-let* ((modes (info-lookup-quick-all-modes 'symbol major-mode)))
    (let ((start (point)) try mode)
      (while (and (not try) modes)
        (setq mode (car modes)
              modes (cdr modes)
              try (info-lookup-guess-default* 'symbol mode))
        (goto-char start))
      (when try
        (-when-let (completions (info-lookup->completions 'symbol mode))
          (when (info-lookup->ignore-case 'symbol mode)
            (setq completions
                  (lambda (string pred action)
                    (let ((completion-ignore-case t))
                      (complete-with-action
                       action completions string pred)))))
          (save-excursion
            (goto-char start)
            (while (and (search-backward try nil t)
                        (< start (point))))
            (list (match-beginning 0) (match-end 0) completions :exclusive 'no)))))))

(provide 'nvp-info-capf)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-info-capf.el ends here
