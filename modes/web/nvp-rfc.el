;;; nvp-rfc.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/web-tools
;; Last modified: <2019-03-28 22:20:56>
;; Created: 30 July 2017

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro))
(require 'irfc nil t)
(declare-function irfc-visit "irfc")
(declare-function irfc-render-buffer-hide-blank-line "irfc")
(declare-function irfc-head-move "irfc")

;; load RFC config prior to visiting rfs
;;;###autoload
(defun nvp-rfc-visit (&optional rfc-number)
  (interactive)
  (irfc-visit rfc-number))

;; -------------------------------------------------------------------
;;; imenu
(defvar imenu-use-markers)
(defvar-local nvp-rfc-imenu-table ())

;; create alist for imenu, set `imenu-create-index-function' within irfc-hook
(defun nvp-rfc-imenu-function ()
  (or nvp-rfc-imenu-table
      (let ((title-line-point (irfc-render-buffer-hide-blank-line (point-min)))
            match-list)
        ;; fill table -- see irfc-fill-tables
        (save-excursion
          (goto-char title-line-point)
          (while (setq match-list (irfc-head-move))
            (push
             (cons
              (if (and (nth 0 match-list) (nth 1 match-list))
                  (buffer-substring-no-properties (nth 0 match-list)
                                                  (nth 3 match-list))
                (buffer-substring-no-properties (nth 2 match-list)
                                                (nth 3 match-list)))
              (if imenu-use-markers (point-marker) (point)))
             nvp-rfc-imenu-table)))
        (nreverse nvp-rfc-imenu-table))))

(provide 'nvp-rfc)
;;; nvp-rfc.el ends here
