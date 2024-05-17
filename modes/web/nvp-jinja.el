;;; nvp-jinja.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decl web-mode-block-beginning web-mode-element-beginning)
(nvp:auto "projectile" 'projectile-project-root)

;; true if in url_for clause and returns the file if it exists
;; TODO: expand to non-static?
;;;###autoload
(defun nvp-jinja-url-for ()
  (when-let* ((root (projectile-project-root))
              (regex (and (web-mode-block-beginning)
                          (looking-at-p "{{")
                          (re-search-forward
                           "\\(static\\).*'\\([^']+\\)'" (line-end-position))))
              (static (match-string 1))
              (file (expand-file-name (match-string 2)
                                      (expand-file-name static root))))
    (and (file-exists-p file) file)))

;;;###autoload
(define-minor-mode jinja-minor-mode "Jinja minor mode"
  :lighter " Jinja"
  (if jinja-minor-mode
      (yas-activate-extra-mode 'jinja-mode)
    (yas-deactivate-extra-mode 'jinja-mode)))

(provide 'nvp-jinja)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-jinja.el ends here
