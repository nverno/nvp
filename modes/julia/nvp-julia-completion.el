;;; nvp-julia-completion.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'company)
(require 'julia-mode)

;; ------------------------------------------------------------
;;* capf for latexsubs

;; (defvar julia--latex-completion-table
;;   (apply-partially #'completion-table-with-predicate
;;                    julia-mode-latexsubs
;;                    (lambda (sym)
;;                      (intern-soft sym julia-mode-latexsubs))
;;                    'strict))

;; Beginnings of a completion at point function
;;;###autoload
(defun nvp-julia-completion-at-point ()
  (let ((bnds (bounds-of-thing-at-point 'symbol)))
    (when bnds
      (cond
       ;; complete latex symbol when current symbol is prefixed
       ;; by '\'
       ((eq (char-before (car bnds)) ?\\)
        (list (1- (car bnds)) (cdr bnds) julia-mode-latexsubs
              :annotation-function
              #'(lambda (arg)
                  (gethash arg julia-mode-latexsubs ""))))))))

;; only complete latex for a moment
;;;###autoload
(defun nvp-julia-latex-complete ()
  (interactive)
  (let ((completion-at-point-functions '(nvp-julia-completion-at-point))
        (company-backends '(company-capf)))
    (company-complete)))

;; -------------------------------------------------------------------
;;; Ring history capf

;; #<marker at 6246 in powershell-completion.el>
;; completion-at-point for ring history
(defun nvp-julia--capf-ring ()
  (let ((end (point-at-eol))
        (start (process-mark (get-buffer-process (current-buffer)))))
    (list start end (ring-elements comint-input-ring))))

(defvar nvp-julia--completion-backends nil)
(defun nvp-julia--capf-snapshot ()
  (unless nvp-julia--completion-backends
    (setq nvp-julia--completion-backends
          `((capf    . ,completion-at-point-functions)
            (company . ,company-backends))))
  (add-hook 'company-completion-finished-hook 'nvp-julia--capf-finished nil t)
  (add-hook 'company-completion-cancelled-hook 'nvp-julia--capf-finished nil t))

(defun nvp-julia--capf-finished (&rest _)
  (remove-hook 'company-completion-finished-hook 'nvp-julia--capf-finished t)
  (remove-hook 'company-completion-cancelled-hook 'nvp-julia--capf-finished t)
  (setq completion-at-point-functions
        (cdr (assq 'capf nvp-julia--completion-backends)))
  (setq company-backends (cdr (assq 'company nvp-julia--completion-backends))))

;; temporarily use ring history for company-completion
;;;###autoload
(defun nvp-julia-ring-complete ()
  (interactive)
  (nvp-julia--capf-snapshot)
  (setq completion-at-point-functions '(nvp-julia--capf-ring))
  (setq company-backends '(company-capf))
  (company-complete))

(provide 'nvp-julia-completion)
;;; nvp-julia-completion.el ends here

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
