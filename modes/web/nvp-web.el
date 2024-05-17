;;; nvp-web.el --- web helpers -*- lexical-binding: t; -*-
;;; Commentary:
;; FIXME: documentation, fixup nav functions
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'web-mode nil t)
(nvp:decls :p (web lsp) :v (lsp-mode web-mode-engine))
(nvp:auto "nvp-jinja" 'nvp-jinja-url-for)
(nvp:auto "projectile" 'projectile-project-root)

(with-eval-after-load 'nvp-repl (require 'nvp-skewer))

;; -------------------------------------------------------------------
;;; Navigation

(defun nvp-web-backward-tag ()
  (interactive)
  (and (eq (char-before) ?>)
       (backward-char))
  (if (eq (char-after) ?<)
      (web-mode-element-previous)
    (web-mode-element-beginning)))

(defun nvp-web-forward-tag ()
  (interactive)
  (if (eq (char-before) ?>)
      (web-mode-element-next)
    (web-mode-element-end)))

;; jump to link when in an <a> or <link> tag
(defun nvp-web-href-at-point ()
  (save-excursion
    (pcase (bound-and-true-p web-mode-engine)
      (`jinja2 (nvp-jinja-url-for))
      (`django (nvp-jinja-url-for))
      (_ (if (not (member (get-text-property (point) 'tag-name) '("a" "link")))
             (user-error "Can't find link at point")
           (if (eq (get-text-property (point) 'tag-type) 'end)
               (web-mode-tag-match)
             (web-mode-element-beginning))
           (and (re-search-forward "href=\"\\(\[^\"\]+\\)" (line-end-position) 'move)
                (match-string 1)))))))

(defun nvp-web-find-href (&optional href)
  (interactive (list (nvp-web-href-at-point)))
  (when href
    (xref-push-marker-stack)
    (find-file (expand-file-name href (projectile-project-root)))))

;; -------------------------------------------------------------------
;;; Help 

(defun nvp-web-help-at-point ()
  (interactive)
  (cond (lsp-mode (call-interactively #'lsp-describe-thing-at-point))
        ((cl-member web-mode-engine '("django" "jinja2") :test 'string=)
         (browse-url "http://jinja.pocoo.org/docs/2.10/"))
        (t (message "TODO"))))

(provide 'nvp-web)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-web.el ends here
