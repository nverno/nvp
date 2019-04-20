;;; nvp-company-abbrev.el --- company backend for abbrevs -*- lexical-binding: t; -*-

;;; Commentary:

;; `company-abbrev' doesn't account for :regexp properties or :enable-function,
;; so doesn't work properly in context, eg. ie "\\degree" or "#inc"

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'abbrev)
(require 'company)
(require 'nvp-abbrev-completion)

;;;###autoload
(defun nvp-company-abbrev (command &optional arg &rest _ignored)
  "`company-mode' completion backend for abbrevs accounting for table props.
Respects abbrev table :regexp and :enable-function properties."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'nvp-company-abbrev))
    (prefix (nvp-abbrev-completion-prefix))
    (candidates (nvp-abbrev-completion-candidates arg 'annotate))
    (meta (abbrev-expansion arg))
    (annotation (or (get-text-property 0 'annotation arg) "<abbrev>"))))

(provide 'nvp-company-abbrev)
;;; nvp-company-abbrev.el ends here
