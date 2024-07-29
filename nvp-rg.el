;;; nvp-rg.el --- rg mods -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Make rg, wgrep-rg, and xterm-color work together.
;; See nvp-ag.el
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'rg)
(require 'nvp-find)
(nvp:req 'nvp-find 'subrs)

(nvp:decls :p (projectile))
(nvp:auto "projectile" projectile-acquire-root)
(declare-function projectile-relevant-known-projects "projectile")

;; Override rg's `compilation-error-regexp-alist' matching
;; to use with `xterm-color-filter'
(defun nvp-rg-match-grouped-filename-xc ()
  (nvp:match-grouped-filename
   nvp-ag/rg-file-column-regex nvp-ag/rg-grouped-file-regex))

;; rg-mode is compilation-derived mode for results
;; this hook lets it work with xterm-color
(defun nvp-rg-setup-xterm ()
  ;; use filter that works with `xterm-color'
  (advice-add 'rg-filter :override #'ignore)
  ;; use `imenu-default-create-index-function'
  (advice-add 'rg-configure-imenu :override #'ignore)
  (setq-local compilation-transform-file-match-alist nil)
  (push 'nvp-rg-group-xc compilation-error-regexp-alist)
  (push (cons 'nvp-rg-group-xc
              (list nvp-ag/rg-file-column-regex
                    'nvp-rg-match-grouped-filename-xc 1 2))
        compilation-error-regexp-alist-alist))

(defun nvp-rg-remove-xterm ()
  (advice-remove 'rg-filter #'ignore)
  (advice-remove 'rg-configure-imenu #'ignore))

(defun nvp-rg-recompile ()
  "Recompile w/o xterm-color filter, using rg defaults."
  (interactive)
  (nvp-rg-remove-xterm)
  (let ((compilation-start-hook
         (delq 'nvp-compilation-start-hook compilation-start-hook))
        (rg-mode-hook (delq 'nvp-rg-mode-hook rg-mode-hook))
        (rg-ignore-ripgreprc t)
        (rg-show-columns t)
        (rg-show-header t)
        (rg-align-line-column-separator ":"))
    (rg-rerun-toggle-flag "--no-config")))

(defun nvp-rg-get-project ()
  (pcase current-prefix-arg
    ('nil (nvp-project-locate-root nil 'local))
    ;; (projectile-acquire-root)
    (`(4) (nvp-project-parent))
    (_ (nvp-completing-read "Project: " (projectile-relevant-known-projects)))))

;;;###autoload(autoload 'nvp-projectile-rg "nvp-project" nil t)
(rg-define-search nvp-projectile-rg
  :query (read-from-minibuffer "Search: " (nvp:tap 'dwim))
  :format (not current-prefix-arg)
  :dir (nvp-rg-get-project)
  :files "everything")

;; Useful function to search the zipped source 
;; https://github.com/dajva/rg.el/issues/69#event-3107793694
(rg-define-search nvp-rg-emacs-source 
  :query (read-from-minibuffer "Search emacs source: " (nvp:tap 'dwim))
  :format (not current-prefix-arg)
  :dir source-directory
  :flags '("-z")
  :files "*.{el,el.gz}"
  :menu ("Custom" "L" "src/emacs"))

(rg-define-search nvp-rg-todos
  :query "\\b(TODO|FIXME|XXX|BUG|HACK|FIX|WIP)\\b"
  :format regexp
  :dir (nvp-rg-get-project)
  :files "everything"
  :menu ("Custom" "T" "Todos"))

(provide 'nvp-rg)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-rg.el ends here
