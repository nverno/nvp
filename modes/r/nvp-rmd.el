;;; nvp-rmd.el --- Rmarkdown helpers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
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
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-rmd.el ends here
