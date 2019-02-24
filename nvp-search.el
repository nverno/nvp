;;; nvp-search.el --- search/replace -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-24 04:52:35>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 13 February 2019

;;; Commentary:
;; todo ag / grep / find all c includes for project
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'hydra)
  (require 'nvp-macro))
(nvp-declare "wgrep" wgrep-exit wgrep-save-all-buffers wgrep-abort-changes
  wgrep-remove-change wgrep-remove-all-change wgrep-toggle-readonly-area
  wgrep-mark-deletion wgrep-change-to-wgrep-mode)

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
