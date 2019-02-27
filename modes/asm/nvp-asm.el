;;; nvp-asm.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-27 02:17:03>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/asm-tools
;; Created: 19 December 2016

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'nvp)

(nvp-package-define-root :snippets t)

;; Download the nasm instruction PDF
(defun nvp-asm-init ()
  (let ((pdf (expand-file-name "intel_dev.pdf" "~/.local/share/doc")))
    (unless (file-exists-p pdf)
      (message "%S" pdf))))

;; -------------------------------------------------------------------
;;; K70 ARM M4-Cortex

;;;###autoload
(defun nvp-k70-help (manual)
  (interactive
   (let ((mans '(("K70 schematics" .
                  "http://cache.freescale.com/files/32bit/hardware_tools/schematics/TWR-K70F120M-SCH_C.pdf")
                 ("Cortex M4" .
                  "http://infocenter.arm.com/help/topic/com.arm.doc.ddi0439b/DDI0439B_cortex_m4_r0p0_trm.pdf")
                 ("Arm Mv7" .
                  "http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0403e.b/index.html")
                 ("J-Link" .
                  "https://www.segger.com/downloads/jlink/UM08001_JLink.pdf")
                 ("J-link Plugin" .
                  "https://gnu-mcu-eclipse.github.io/debug/jlink/"))))
     (list (assoc-default (nvp-completing-read "Manual: " mans) mans))))
  (browse-url manual))

(provide 'nvp-asm)
;;; nvp-asm.el ends here
