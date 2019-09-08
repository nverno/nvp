;;; nvp-rfc.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro))
(require 'irfc nil t)
(nvp-decl "irfc" irfc-visit irfc-render-buffer-hide-blank-line irfc-head-move) 

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
