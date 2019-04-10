;;; nvp-org-convert.el --- conversions -*- lexical-binding: t; -*-

;; Last modified: <2019-04-10.08>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 10 April 2019

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'nvp)

(defun nvp-convert-csv-to-org-table (fname &optional sep)
  "Rough conversion from CSV to org-table.
Not robust at all, doesn't account for quoted fields."
  (interactive "fCSV to convert: ")
  (or sep (setq sep ","))
  (with-temp-buffer
    (save-excursion (insert-file-contents-literally fname))
    (insert "|")
    (while (not (eobp))
      (while (search-forward sep (line-end-position) 'move)
        (replace-match "|" nil t))
      (insert "|")
      (forward-line 1))
      (buffer-string)))

(provide 'nvp-org-convert)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-org-convert.el ends here
