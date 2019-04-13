;;; nvp-parse.el --- Generics for parse -*- lexical-binding: t; -*-

;;; Commentary:

;; TODO:
;; - cache values by window -- see which-func
;; #<marker at 10467 in which-func.el.gz>
;; - use sematic when tags available:
;; #<marker at 19142 in semantic/imenu.el.gz>

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'nvp-imenu)

;; -------------------------------------------------------------------
;;; utils

(eval-when-compile
  ;; imenu alists can be nested under headers:
  ;; ("Header" ("name" . marker)) or ("name" . marker)
 (defmacro nvp--parse-ensure-imenu ()
   "Create imenu index alist if possible."
   `(ignore-errors
      (when (and (fboundp 'imenu--make-index-alist) (null imenu--index-alist))
        (setq imenu--index-alist
              (save-excursion (funcall imenu-create-index-function)))))))

;; -------------------------------------------------------------------
;;; Generics

(defmacro nvp-parse-with-buffer-or-file (args &rest body)
  "Execute BODY in buffer or file specified in ARGS or the current buffer.
Kills buffer if opened."
  (declare (indent 1) (debug (sexp &rest form)))
  (macroexp-let2 nil pargs args
   `(let* ((opened-p)
           (buff (or (plist-get ,pargs :buffer)
                     (when-let ((fname (plist-get ,pargs :filename)))
                       (or (buffer-file-name fname)
                           (prog1 (find-file-noselect fname)
                             (setq opened-p t))))
                     (current-buffer))))
      (with-current-buffer buff
        (prog1 (progn ,@body)
          (and opened-p (kill-this-buffer)))))))

;; default just tries to use imenu
(cl-defgeneric nvp-parse-function-names (&rest args)
  "Default method to gather function names from current buffer.
Additionally recognized ARGS to specify locations to gather functions from:
 - :buffer 
 - :filename"
  ;; just loops through alist and gathers names
  (nvp-parse-with-buffer-or-file args
    (nvp--parse-ensure-imenu)
    (nvp-imenu-cleaned-alist)))

;; like which-func - attempt with imenu and add-log
(cl-defgeneric nvp-parse-current-function (&rest _args)
  "Default method to get name of function containing point.
First tries closest imenu entry, then `add-log-current-defun'."
  (nvp--parse-ensure-imenu)
  (let ((func (nvp-imenu-sort-relative-positions (point) (nvp-imenu-cleaned-alist))))
    (if func (caar func)
      (add-log-current-defun))))

(cl-defgeneric nvp-parse-library (&rest _args)
  "Generic function to return the name of the current library, module, ..."
  nil)

(cl-defgeneric nvp-parse-includes (&rest _args)
  "Generic function to return the names of required libraries."
  nil)

(provide 'nvp-parse)
;;; nvp-parse.el ends here
