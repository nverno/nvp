;; -*- lexical-binding: t; -*-
;; -------------------------------------------------------------------
;;; Help at point 

;; doc for symbol at point
;; (defun nvp-scheme-help--doc-string (sym &optional module impl)
;;   (let* ((impl (or impl geiser-impl--implementation))
;;          (module (geiser-doc--module (or module (geiser-eval--get-module))
;;                                      impl))
;;          (ds (geiser-doc--get-docstring sym module)))
;;     (if (or (not ds) (not (listp ds)))
;;         (format "No documentation available for '%s'" sym)
;;       (substring (cdr (assoc "docstring" ds)) 0 -1))))

;; -------------------------------------------------------------------
;;; scheme help minor mode

;;; Doc-mode, not used anymore -- not sure wy I needed it
;; (defun nvp-scheme-help-next-link ()
;;   (interactive)
;;   (let ((plink
;;          (let ((pnt (next-single-char-property-change (point) 'category)))
;;            (while (not (or (eq pnt (point-max))
;;                            (get-text-property pnt 'category)))
;;              (setq pnt (next-single-char-property-change pnt 'category)))
;;            pnt)))
;;     (if (not (eq plink (point-max)))
;;         (goto-char plink)
;;       (unless (eq plink (point-min))
;;         (goto-char (point-min))
;;         (nvp-scheme-help-next-link)))))

;; (defvar nvp-scheme-help-doc-mode-map
;;   (let ((km (make-sparse-keymap)))
;;     (define-key km (kbd "TAB") 'nvp-scheme-help-next-link)
;;     km))

;; (define-minor-mode nvp-scheme-help-doc-mode ""
;;   :lighter " Doc"
;;   :keymap 'nvp-scheme-help-doc-mode-map
;;   (view-mode-enter))

