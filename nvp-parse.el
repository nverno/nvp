;;; nvp-parse.el --- Generics for parse -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; TODO:
;; - cache values by window -- see which-func
;; #<marker at 10467 in which-func.el.gz>
;; - use sematic when tags available:
;; #<marker at 19142 in semantic/imenu.el.gz>
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:auto "nvp-imenu" 'nvp-imenu-sort-relative-positions 'nvp-imenu-cleaned-alist)
(nvp:decls)

(put 'nvp-parse-bad-location 'error-conditions '(nvp-parse-bad-location error))
(put 'nvp-parse-bad-location 'error-message "Location not available: ")

;; imenu alists can be nested under headers:
;; ("Header" ("name" . marker)) or ("name" . marker)
(defsubst nvp-parse--ensure-imenu ()
  "Create imenu index alist if possible."
  (when (and (fboundp 'imenu--make-index-alist)
             (boundp 'imenu--index-alist))
    (or imenu--index-alist
        (setq imenu--index-alist
              (save-excursion
                (ignore-errors (funcall imenu-create-index-function)))))))

(defmacro nvp-parse:buffer-file (want-buff error pargs &rest body)
  "Parse :buffer and :file arguments.
If WANT-BUFF, do BODY inside the found buffer. If the buffer was opened,
it is killed after BODY. The buffer is found by:
  (1) :buffer is non-nil and active
  (2) :file is non-nil and already opened in a buffer
  (3) :file is non-nil, open a buffer with `find-file-noselect'
  (4) if ERROR is nil, default to `current-buffer', otherwise error

If WANT-BUFF is nil, BODY is executed with the symbol FILE bound to the
found file.  File is determined by:
  (1) :file is non-nil and exists
  (2) :buffer is non-nil and is attached to a file
  (3) if ERROR is nil, default to `buffer-file-name' of current buffer
  (4) finally error if default fails
"
  (declare (indent defun) (debug t))
  (nvp:with-syms (args buff file opened-p)
    `(let ((,args ,pargs))
       ,(if want-buff
            `(-let* (((&plist :buffer ,buff :file ,file) ,args)
                     (,opened-p)
                     (,buff
                      (if (and ,buff (setq ,buff (get-buffer ,buff))) ,buff
                        (if ,file
                            (or (get-file-buffer ,file)
                                (prog1 (find-file-noselect ,file)
                                  (setq ,opened-p t)))
                          ,(if error
                               '(signal 'nvp-parse-bad-location (or ,buff ,file))
                             '(current-buffer))))))
               (with-current-buffer ,buff
                 (prog1 (progn ,@body)
                   (and ,opened-p (kill-current-buffer)))))
          `(-let* (((&plist :buffer ,buff :file file) ,args)
                   (file (if (and file (file-exists-p file)) file
                           (or (and ,buff (setq ,buff (get-buffer ,buff))
                                    (buffer-file-name ,buff))
                               ,(when (not error)
                                  '(buffer-file-name))
                               (signal 'nvp-parse-bad-location (or file ,buff))))))
             (progn ,@body))))))


;;; Generics

;;;###autoload
(cl-defgeneric nvp-parse-functions (&optional _mode &rest args)
  "Default method to gather function names from current buffer.
Recognized arguments:
  :buffer <buffer-or-name> - location to search
  :filename <filename>     - location to search
  :local t                 - find functions local to location"
  ;; just loops through alist and gathers names
  (nvp-parse:buffer-file t nil args
    (nvp-parse--ensure-imenu)
    (mapcar #'car (nvp-imenu-cleaned-alist))))

(cl-defgeneric nvp-parse-variables (&optional _mode &rest _args)
  "Gather variables from buffer or file.
Recognized arguments:
 :buffer <buffername> to search
 :file <filename> to search
 :local <point> - find local variables at POINT"
  nil)
;; (cl-defmethod nvp-parse-variables ((_mode (eql nil)) &rest args)
;;   (apply #'cl-call-next-method (or nvp-mode-name major-mode) args))

(cl-defgeneric nvp-parse-library (&optional _mode &rest _args)
  "Generic function to return the name of the current library, module, ..."
  nil)
;; (cl-defmethod nvp-parse-library ((_mode (eql nil)) &rest args)
;;   (apply #'nvp-parse-library (or nvp-mode-name major-mode) args))

(cl-defgeneric nvp-parse-includes (&optional _mode &rest _args)
  "Generic function to return the names of required libraries."
  nil)

;;;###autoload
(cl-defgeneric nvp-parse-current-function (&rest _args)
  "Default method to get name of function containing point.
First tries closest imenu entry, then `add-log-current-defun'."
  (nvp-parse--ensure-imenu)
  (let ((func (nvp-imenu-sort-relative-positions (point) (nvp-imenu-cleaned-alist))))
    (if func (caar func)
      (add-log-current-defun))))


;; -------------------------------------------------------------------
;;; Commands

;;;###autoload
(defun nvp-show-current-function ()
  (interactive)
  (-if-let (func (nvp-parse-current-function))
      (message "Current function: %s" func)
    (message "No current function found.")))

(provide 'nvp-parse)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-parse.el ends here
