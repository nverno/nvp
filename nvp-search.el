;;; nvp-search.el --- search/replace -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-08 15:36:31>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 13 February 2019

;;; Commentary:
;; todo ag / grep / find all c includes for project
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'hydra)
  (require 'nvp-macro)
  (nvp-local-vars))
(nvp-declare "wgrep" wgrep-exit wgrep-save-all-buffers wgrep-abort-changes
  wgrep-remove-change wgrep-remove-all-change wgrep-toggle-readonly-area
  wgrep-mark-deletion wgrep-change-to-wgrep-mode)

;; -------------------------------------------------------------------
;;; Rgrep

;;;###autoload
(defun nvp-rgrep-symbol (arg)
  "Lookup SYM using rgrep with FILES regexp from ROOT directory.
By default, lookup symbol at point in files matching current file's extension
or '*' from emacs root, ignoring package directory.
With single prefix, includes package directory.
With double prefix just calls `rgrep' interactively to prompt for defaults."
  (interactive "P")
  (require 'grep)
  (if (equal '(16) arg) (call-interactively 'rgrep)
    (let ((sym (or (thing-at-point 'symbol t)
                   (read-from-minibuffer "Symbol: ")))
          (ext (file-name-extension (or (buffer-file-name) (buffer-name))))
          (grep-find-ignored-directories
           (if (not (equal '(4) arg))
               (cons (file-name-nondirectory package-user-dir)
                     grep-find-ignored-directories)
             grep-find-ignored-directories)))
      (grep-compute-defaults)
      (rgrep (concat "\\<" sym "\\>") (if ext (concat "*." ext) "*") nvp/emacs))))

;; -------------------------------------------------------------------
;;; wgrep

;;;###autoload
(defun nvp-wgrep-bind ()
  "Bind wgrep in current mode."
  (interactive)
  (require 'nvp-grep-config)
  (let ((map (symbol-value (intern-soft (format "%s-map" major-mode)))))
    (define-key map (kbd "C-x C-n w") #'wgrep-change-to-wgrep-mode)))

;;;###autoload(autoload 'nvp-wgrep-hydra/body "nvp-search")
(nvp-hydra-set-property 'nvp-wgrep-hydra)
(defhydra nvp-wgrep-hydra (:color red)
  ("q" wgrep-exit "exit")
  ("s" wgrep-save-all-buffers "save all")
  ("a" wgrep-abort-changes "abort")
  ("r" wgrep-remove-change "remove region change")
  ("R" wgrep-remove-all-change "remove all changes")
  ("t" wgrep-toggle-readonly-area "toggle r/o")
  ("m" wgrep-mark-deletion "mark deletion"))

(provide 'nvp-search)
;;; nvp-search.el ends here
