;;; nvp-doc.el --- doc strings/buffers -*- lexical-binding: t; -*-

;; Last modified: <2019-03-27 15:48:07>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 10 February 2019

;;; Commentary:

;; Generic documentation lookup, modeled after xref
;; popup.el/pos-tip.el/quickhelp.el to truncate pop-tips
;; #<marker at 84732 in etags.el.gz>
;; #<marker at 8769 in cl-generic.el.gz>

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(declare-function pos-tip-show "pos-tip")

;; -------------------------------------------------------------------
;;; Tooltips

(eval-when-compile
  ;; TODO:
  ;; - use help buffer with xref?
  ;; - better popup formatting
  (cl-defmacro nvp-with-toggled-tip (popup
                                     &key
                                     (help-key "h") ;key-binding for help-fn
                                     (help-fn t)    ;more help (t is default)
                                     bindings       ;additional bindings
                                     (timeout 45)   ;pos-tip timeout
                                     keep           ;keep transient map
                                     use-gtk        ;use gtk tooltips
                                     (help-buffer '(help-buffer)))
    "Toggle POPUP, a help string, in pos-tip.
If HELP-FN is :none, HELP-KEY is not bound by default.
Normally, HELP-KEY triggers a function to jump to a full help description
related to the popup - hopefully in a buffer.
BINDINGS are an alist of (key . function) of additional keys to bind in the
transient keymap.
TIMEOUT is passed to `pos-tip-show'.
If USE-GTK is non-nil use gtk tooltips.
KEEP is passed to `set-transient-map'.
HELP-BUFFER is buffer with full help documentation. This is only applicable to the
default help function."
    (declare (indent defun) (debug t))
    (macroexp-let2* nil ((str popup)
                         (h-key (or help-key "h"))
                         (h-fn (cond
                                ((eq :none help-fn) nil) ;back-compat
                                ((eq t help-fn)
                                 `(lambda ()
                                    (interactive)
                                    (x-hide-tip)
                                    (with-help-window ,help-buffer
                                      (with-current-buffer standard-output
                                        (insert ,str)))
                                    ;; (with-current-buffer ,help-buffer
                                    ;;   (insert ,str)
                                    ;;   (view-mode-enter)
                                    ;;   (pop-to-buffer (current-buffer)))
                                    ))
                                (help-fn help-fn)
                                (t nil)))
                         (exit-fn '(lambda () (interactive) (x-hide-tip)))
                         (keep keep))
      `(progn
         (declare-function pos-tip-show "pos-tip")
         (let ((x-gtk-use-system-tooltips ,use-gtk))
           (unless (x-hide-tip)
             (pos-tip-show ,str nil nil nil ,timeout)
             (set-transient-map
              (let ((tmap (make-sparse-keymap)))
                (define-key tmap ,h-key ,h-fn)
                ,@(cl-loop for (k . b) in bindings
                     collect `(define-key tmap ,k ,b))
                tmap)
              ,keep
              ,exit-fn)))))))

;; -------------------------------------------------------------------
;;; Doc backends

(cl-defstruct (nvp-doc (:constructor nvp-doc-make)
                       (:copier nil))
  "Documentation location information.
Location could be a buffer, file, or a function to call with no args.
START and END can be specify relevant region."
  buffer file func start end)

;;; TODO: default to getting capf / company documentation
(defvar nvp-doc-backend-functions nil
  "Special hook to find applicable doc backend for context.")

;;;###autoload
(defun nvp-doc-find-backend ()
  (run-hook-with-args-until-success 'nvp-doc-backend-functions))

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
  (let* ((backend (nvp-doc-find-backend))
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
                      (nvp-doc-find-backend)
                      arg)))
    (unless doc
      (user-error "No %s found for: %s" (symbol-name kind) input))
    (nvp-doc--show-doc doc display-action)))

(provide 'nvp-doc)
;;; nvp-doc.el ends here
