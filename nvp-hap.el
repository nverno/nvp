;;; nvp-hap.el --- help-at-point functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Help at point:
;; - display help for context around point in popup window
;; - use transient bindings to execute actions from there
;; popup.el/pos-tip.el/quickhelp.el to truncate pop-tips
;; #<marker at 84732 in etags.el.gz>
;; #<marker at 8769 in cl-generic.el.gz>

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(declare-function company-quickhelp-manual-begin "company-quickhelp")

(defvar nvp-hap-functions nil
  "Special hook to find first applicable help at point.")

;;;###autoload
(defun nvp-hap-find-backend ()
  (run-hook-with-args-until-success 'nvp-hap-functions))

;;; Toggle
;;;###autoload
(defun nvp-company-quickhelp-toggle ()
  "Toggle pos-tip help on/off."
  (interactive)
  (let ((x-gtk-use-system-tooltips nil))
    ;; tell `company-quickhelp--manual-begin' to start the timer
    ;; An alternative is calling `company-quickhelp--cancel-timer' instead, but
    ;; I find it buggy when toggling quickly (the popup stops working until
    ;; moving on to another candidate - I believe breaking the `while-no-input'
    ;; cycle, but not really sure)
    ;; #<marker at 8895 in company-quickhelp.el>
    (nvp-toggled-if
      (if-let ((fn (bound-and-true-p nvp-doc-toggle-fn)))
          (funcall fn 'company)
        (company-quickhelp-manual-begin))
      :this-cmd 'company-quickhelp-manual-begin
      (x-hide-tip))))

;; -------------------------------------------------------------------
;;; Backends

(cl-defstruct (nvp-doc (:constructor nvp-doc-make)
                       (:copier nil))
  "Documentation location information.
Location could be a buffer, file, or a function to call with no args.
START and END can be specify relevant region."
  buffer file func start end)


(cl-defgeneric nvp-doc-backend-identifier-at-point (_backend)
  "Return identifier at point, a string or nil."
  (when-let ((ident (thing-at-point 'symbol)))
    (substring-no-properties ident)))

(cl-defgeneric nvp-doc-backend-identifier-completion-table (_backend)
  "Returns the completion table for identifiers."
  nil)

(cl-defgeneric nvp-doc-backend-doc (backend identifier)
  "Return doc for IDENTIFIER.")

(defvar nvp-doc--read-identifier-history nil)

(defun nvp-doc--read-identifier (prompt)
  "Return identifier at point or read from minibuffer."
  (let* ((backend (nvp-hap-find-backend))
         (id (nvp-doc-backend-identifier-at-point)))
    (cond ((or current-prefix-arg (not id))
           (completing-read
            (if id (format "%s (default %s): "
                           (substring prompt 0 (string-match "[ :]+\\'" prompt))
                           id)
              prompt)
            (nvp-doc-backend-identifier-completion-table backend)
            nil nil nil
            'nvp-doc--read-identifier-history id))
          (t id))))

;;;###autoload
(defun nvp-doc-at-point (identifier &optional arg action)
  (interactive
   (list (nvp-doc--read-identifier "Documentation for: ") current-prefix-arg t))
  (nvp-doc--find-doc identifier 'popup arg action))

(defun nvp-doc--show-doc (_doc _display-action)
  "Show DOC according to DISPLAY-ACTION.")

(defun nvp-doc--find-doc (input kind arg display-action)
  (let ((doc (funcall (intern (format "nvp-doc-backend-%s" kind))
                      (nvp-hap-find-backend)
                      arg)))
    (unless doc
      (user-error "No %s found for: %s" (symbol-name kind) input))
    (nvp-doc--show-doc doc display-action)))

;; -------------------------------------------------------------------
;;; Company backend
(defun nvp-doc--company-backend () 'company)
(cl-defmethod nvp-doc-backend-identifier-at-point ((_backend (eql company)))
  
  (nth company-selection company-candidates))

(provide 'nvp-hap)
;;; nvp-hap.el ends here
