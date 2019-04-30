;;; nvp-results.el --- viewing results -*- lexical-binding: t; -*-

;;; Commentary:

;; View formatted results in temporary buffers:
;; - help-buffers
;; - tabulated lists
;; refs:
;; - timer-list.el
;; - view-list #<marker at 153343 in evil-common.el>

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'nvp)
(nvp-auto "nvp-string" nvp-s-repeat nvp-s-center)
(nvp-decls)

;; -------------------------------------------------------------------
;;; Utils

;; princ centered TITLE
(defsubst nvp-results--princ-title (title &optional width char)
  (or width (setq width 85))
  (or char (setq char "~"))
  (princ (format "\n%s\n%s\n\n" (nvp-s-center (- width (length title)) title)
                 (nvp-s-repeat width "~"))))

;; -------------------------------------------------------------------
;;; View list - simple tabulated display

(defvar-local nvp-view-list-select-action ())
(put 'nvp-view-list-select-action 'permanent-local t)

(defun nvp-view-list-goto-entry ()
  (interactive)
  (when (and nvp-view-list-select-action
             (not (eobp)))
    (let* ((line (line-number-at-pos (point)))
           (entry (elt tabulated-list-entries (1- line))))
      (funcall nvp-view-list-select-action (nth 1 entry)))))

;;;###autoload
(define-derived-mode nvp-view-list-mode tabulated-list-mode
  "Simple list view."
  (tabulated-list-init-header)
  (tabulated-list-print))

(nvp-bindings-with-view "nvp-view-list" nil
  ([return] . nvp-view-list-goto-entry)
  ("q"      . kill-this-buffer))

(provide 'nvp-results)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-results.el ends here
