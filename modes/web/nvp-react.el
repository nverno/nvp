;;; nvp-react.el --- setup for react -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'web-mode)
(nvp-decls :v (emmet-expand-jsx-className?))

;;;###autoload
(defun nvp-web-setup-react ()
  (interactive)
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
