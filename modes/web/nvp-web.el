;;; nvp-web.el --- web helpers -*- lexical-binding: t; -*-
;;; Commentary:
;; FIXME: documentation, fixup nav functions
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'web-mode nil t)
(nvp:decls :p (web httpd impatient lsp) :v (lsp-mode))
(nvp:auto "nvp-jinja" 'nvp-jinja-url-for)
(nvp:auto "projectile" 'projectile-project-root)

;; -------------------------------------------------------------------
;;; Serve buffer interactively

;; `imp-visit-buffer' doesn't seem to work for me
;; is `system-name' supposed to be handled in the browser correctly?
(defun nvp-web-browse-buffer (&optional arg)
  (interactive "P")
  (unless (httpd-running-p)
    (let ((httpd-root default-directory))
      (httpd-start)))
  (unless (impatient-mode) (impatient-mode))
  (let ((url (format "http://localhost:%d/imp/" httpd-port)))
    (unless arg
      (setq url (format "%slive/%s/" url
                        (url-hexify-string (buffer-name)
                                           ;; (file-name-nondirectory (buffer-name))
                                           ))))
    (browse-url url)))

;; https://github.com/cjohansen/.emacs.d/blob/master/defuns/misc-defuns.el
;; Start httpd-server in current directory.
(defun nvp-web-httpd-start-here (directory port)
  (interactive
   (list (read-directory-name "Root directory: " default-directory nil t)
         (read-number "Port: " 8017)))
  (setq httpd-root directory
        httpd-port port
        httpd-host 'local)
  (httpd-start)
  (browse-url (concat "http://localhost:" (number-to-string port) "/")))

;; doesn't recognize web-mode as an html-mode and unecessarily does some shit
(define-advice skewer-html-eval-tag (:around (fn &rest args) "html-mode")
  (let ((major-mode 'html-mode))
    (apply fn args)))

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
  (cond (lsp-mode (call-interactively 'lsp-describe-thing-at-point))
        ((cl-member web-mode-engine '("django" "jinja2") :test 'string=)
         (browse-url "http://jinja.pocoo.org/docs/2.10/"))
        (t (message "TODO"))))

(provide 'nvp-web)
;;; nvp-web.el ends here
