;;; nvp-rg.el --- rg mods -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Make rg, wgrep-rg, and xterm-color work together.
;; See nvp-ag.el
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:req 'nvp-find 'subrs)
(require 'rg)
(require 'nvp-find)
(require 'nvp)
(nvp:decls :v (projectile-globally-ignored-files
               projectile-globally-ignored-directories))

;; Override rg's `compilation-error-regexp-alist' matching
;; to use with `xterm-color-filter'
(defun nvp-rg-match-grouped-filename-xc ()
  (nvp:match-grouped-filename
   nvp-ag/rg-file-column-regex nvp-ag/rg-grouped-file-regex))

;; use my own filter that works with `xterm-color'
(advice-add 'rg-filter :override #'ignore)
;; (defalias 'rg-filter 'ignore)

;; rg-mode is compilation-derived mode for results
;; this hook lets it work with xterm-color
(defun nvp-rg-setup-xterm ()
  (setq-local compilation-transform-file-match-alist nil)
  (push 'nvp-rg-group-xc compilation-error-regexp-alist)
  (push (cons 'nvp-rg-group-xc
              (list nvp-ag/rg-file-column-regex
                    'nvp-rg-match-grouped-filename-xc 1 2))
        compilation-error-regexp-alist-alist))

;; (defun nvp-rg-toplevel ()
  
;;   )

;; search in toplevel project
;; (rg-define-search nvp-rg-toplevel
;;   :query (read-from-minibuffer "Search toplevel: " (nvp:tap 'dwim))
;;   :literal (not current-prefix-arg)
;;   :dir 
;;   )

;;;###autoload(autoload 'nvp-projectile-rg "nvp-project")
(rg-define-search nvp-projectile-rg
  :query (read-from-minibuffer "Search: " (nvp:tap 'dwim))
  :dir project
  :format (not current-prefix-arg)
  :files (concat
          (mapconcat
           #'identity
           (--map (concat "--glob !" it)
                  (append projectile-globally-ignored-files
                          projectile-globally-ignored-directories))
           " "))
  :flags '("--type all"))

;; Useful function to search the zipped source 
;; https://github.com/dajva/rg.el/issues/69#event-3107793694
(rg-define-search nvp-rg-emacs-source 
  :query (read-from-minibuffer "Search emacs source: " (nvp:tap 'dwim))
  :literal (not current-prefix-arg)
  :dir source-directory
  :flags '("-z")
  :files "*.{el,el.gz}"
  :menu ("Custom" "L" "src/emacs"))


(provide 'nvp-rg)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-rg.el ends here
