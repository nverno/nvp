;;; nvp-scheme-help.el --- scheme help -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'geiser)
(require 'geiser-doc)
(nvp:decls :v (geiser-impl--implementation)
           :f (geiser-eval--get-module))
(nvp:auto "pos-tip" 'pos-tip-show)

;; doc for symbol at point
(defun nvp-scheme-help--doc-string (sym &optional module impl)
  (let* ((impl (or impl geiser-impl--implementation))
         (module (geiser-doc--module (or module (geiser-eval--get-module))
                                     impl))
         (ds (geiser-doc--get-docstring sym module)))
    (if (or (not ds) (not (listp ds)))
        (format "No documentation available for '%s'" sym)
      (substring (cdr (assoc "docstring" ds)) 0 -1))))

(eval-when-compile
  ;; cycle between links in doc buffer
  (defmacro nvp-scheme-help--next-link ()
    `(let ((pnt (next-single-char-property-change (point) 'category)))
       (while (not (or (eq pnt (point-max))
                       (get-text-property pnt 'category)))
         (setq pnt (next-single-char-property-change pnt 'category)))
       pnt)))

;;; Doc-mode, not used anymore -- not sure wy I needed it
(defun nvp-scheme-help-next-link ()
  (interactive)
  (let ((plink (nvp-scheme-help--next-link)))
    (if (not (eq plink (point-max)))
        (goto-char plink)
      (unless (eq plink (point-min))
        (goto-char (point-min))
        (nvp-scheme-help-next-link)))))

(defvar nvp-scheme-help-doc-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "TAB") 'nvp-scheme-help-next-link)
    km))

(define-minor-mode nvp-scheme-help-doc-mode ""
  :lighter " Doc"
  :keymap 'nvp-scheme-help-doc-mode-map
  (view-mode-enter))

;; -------------------------------------------------------------------
;;; Commands

;; help in popup tooltip for thing at point
;;;###autoload
(defun nvp-scheme-help-at-point (sym)
  (interactive (list (symbol-at-point)))
  (let ((str (nvp-scheme-help--doc-string sym))
        (sym sym))
    (nvp:with-toggled-tip str
      :help-fn #'(lambda ()
                   (interactive)
                   (x-hide-tip)
                   (geiser-doc-symbol sym)
                   ;; (geiser-company--doc-buffer (symbol-name sym))
                   ;; (pop-to-buffer "*company-documentation*")
                   ;; (nvp-scheme-help-doc-mode)
                   ))))

;;;###autoload
(defun nvp-scheme-help-company-show-doc ()
  "Show the doc for current company selection."
  (interactive)
  (let ((selected (nth company-selection company-candidates)))
    (geiser-doc--external-help geiser-impl--implementation
                               selected (geiser-eval--get-module))))


(provide 'nvp-scheme-help)
;;; nvp-scheme-help.el ends here
