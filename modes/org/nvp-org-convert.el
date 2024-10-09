;;; nvp-org-convert.el --- conversions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))

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
