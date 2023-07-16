;;; nvp-scheme-help.el --- scheme help -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'geiser nil t)
(require 'geiser-doc nil t)
(nvp:decls :v (geiser-impl--implementation)
           :f (geiser-eval--get-module
               geiser-doc--external-help geiser-doc--module geiser-doc--get-docstring
               geiser-doc-symbol))
(nvp:auto "nvp-hap" nvp-hap-thing-at-point)

;;;###autoload
(defun nvp-scheme-help-company-show-doc ()
  "Show the doc for current company selection."
  (interactive)
  (let ((selected (nth company-selection company-candidates)))
    (geiser-doc--external-help geiser-impl--implementation
                               selected (geiser-eval--get-module))))

;; -------------------------------------------------------------------
;;; Help at point 

;; doc for symbol at point
(defun nvp-scheme-help--doc-string (sym &optional module impl)
  (let* ((impl (or impl geiser-impl--implementation))
         (module (geiser-doc--module (or module (geiser-eval--get-module))
                                     impl))
         (ds (geiser-doc--get-docstring sym module)))
    (if (or (not ds) (not (listp ds)))
        (format "No documentation available for '%s'" sym)
      (substring (cdr (assoc "docstring" ds)) 0 -1))))

;;;###autoload
(defun nvp-hap-scheme (command &optional arg &rest _args)
  (cl-case command
    (thingatpt (intern (nvp-hap-thing-at-point arg)))
    (doc-string (nvp-scheme-help--doc-string arg))
    (doc-buffer
     (save-window-excursion
       (geiser-doc-symbol arg)
       (list (current-buffer) (point-min))))))

;; -------------------------------------------------------------------
;;; FIXME: unused - scheme help minor mode

;;; Doc-mode, not used anymore -- not sure wy I needed it
(defun nvp-scheme-help-next-link ()
  (interactive)
  (let ((plink
         (let ((pnt (next-single-char-property-change (point) 'category)))
           (while (not (or (eq pnt (point-max))
                           (get-text-property pnt 'category)))
             (setq pnt (next-single-char-property-change pnt 'category)))
           pnt)))
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

(provide 'nvp-scheme-help)
;;; nvp-scheme-help.el ends here
