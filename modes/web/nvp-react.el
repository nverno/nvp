;;; nvp-react.el --- setup for react -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'web-mode nil t)
(nvp:decls :p (emmet yas web) :v (web-mode-set-content-type))

;;;###autoload
(define-minor-mode react-minor-mode "React"
  :lighter " ℝ"
  (if react-minor-mode
      (let ((yas-dir (expand-file-name "web/snippets/react-mode" nvp/modes)))
        (unless (member yas-dir yas-snippet-dirs)
          (push yas-dir yas-snippet-dirs)
          (yas-load-directory yas-dir))
        (yas-activate-extra-mode 'react-mode))
    (yas-deactivate-extra-mode 'react-mode)))

;;;###autoload
(defun nvp-web-setup-react ()
  (interactive)
  (react-minor-mode)
  (setq-local emmet-expand-jsx-className? t)
  (web-mode-set-content-type "jsx")
  ;; don't auto-quote attribute values
  (setq-local web-mode-enable-auto-quoting nil))

(provide 'nvp-react)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-react.el ends here
