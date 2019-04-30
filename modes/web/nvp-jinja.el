;;; nvp-jinja.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'subr-x)
  (require 'nvp-macro))
(nvp-decl web-mode-block-beginning web-mode-element-beginning)
(nvp-auto "projectile" 'projectile-project-root)

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

(provide 'nvp-jinja)
;;; nvp-jinja.el ends here
