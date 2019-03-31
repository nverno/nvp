;;; nvp-rmd.el --- Rmarkdown helpers -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/r-tools
;; Last modified: <2019-03-30 22:46:36>
;; Created:  4 December 2016

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (require 'polymode-core nil t)
  (defvar pm/chunkmode))
(declare-function pm-with-narrowed-to-span "polymode")
(declare-function pm-map-over-spans "polymode-core")
(declare-function pm-narrow-to-span "polymode-core")

(autoload 'eieio-oref "eieio-core")
(autoload 'ess-eval-region "ess")
(eieio-declare-slot :mode)

;; Send R code regions from polymode
(defun nvp-rmd-send-chunk ()
  "Send current R chunk to ess process."
  (interactive)
  (and (eq (oref pm/chunkmode :mode) 'r-mode)
       (pm-with-narrowed-to-span nil
         (goto-char (point-min))
         (forward-line)
         (ess-eval-region (point) (point-max) nil nil 'R))))

(defun nvp-rmd-send-buffer (arg)
  "Send all R code blocks in buffer to ess process. With prefix
send regions above point."
  (interactive "P")
  (save-restriction
    (widen)
    (save-excursion
      (pm-map-over-spans
       'nvp-rmd-send-chunk (point-min)
       (if arg (point) (point-max))))))

(provide 'nvp-rmd)
;;; nvp-rmd.el ends here
