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
  (require 'nvp-macro))
(require 'nvp)
(nvp-auto "nvp-util" nvp-s-repeat nvp-s-center)
(nvp-decls)

;; -------------------------------------------------------------------
;;; Pretty printing

;; princ centered TITLE
(defsubst nvp-results--princ-title (title &optional width char)
  (or width (setq width 85))
  (or char (setq char "~"))
  (princ (format "\n%s\n%s\n\n" (nvp-s-center (- width (length title)) title)
                 (nvp-s-repeat width "~"))))

;; see `ido-pp' && `smex-pp'
;; (defun nvp-pp-list (list)
;;   (let (print-level eval-expression-print-level print-length
;;                     eval-expression-print-length)
;;     (while list
;;       (let* ((elt (car list))
;;              (s (if (consp elt) (car elt) elt)))
;;         (if (and (stringp s) (= (length s) 0))
;;             (setq s nil))
;;         (if s
;;             (prin1 elt (current-buffer)))
;;         (if (and (setq list (cdr list)) s)
;;             (insert "\n "))))
;;     (insert "\n)\n")))

(defun nvp-pp-hash (hash)
  (maphash (lambda (key val)
             (pp key)
             (princ " => ")
             (pp val)
             (terpri))
           (symbol-value hash)))

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
