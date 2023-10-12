;;; nvp-transient.el --- transient dev -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Add/remove dev commands from transient menus
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'transient)

(transient-define-suffix nvp-transient--args ()
  "Show current infix args."
  (interactive)
  (message "%S" (transient-args transient-current-command)))

(eval-when-compile
  (defmacro nvp:pp-results (obj &optional title)
    `(nvp:with-results-buffer
       :title ,title
       (nvp:pp-object ,obj (current-buffer)))))

(transient-define-suffix nvp-transient--layout ()
  "Show layout."
  (interactive)
  (let* ((cmd transient-current-command)
         (layout (get cmd 'transient--layout)))
    (nvp:pp-results layout (format "`%S' Layout" cmd))))

(transient-define-suffix nvp-transient--object ()
  "Show object."
  (interactive)
  (nvp:pp-results transient-current-prefix
                 (format "%S" transient-current-command)))

(defvar nvp-transient--dev-suffixes
  '(("a" "Infix args" nvp-transient--args :transient t)
    ("l" "Layout" nvp-transient--layout :transient t)
    ("o" "Object" nvp-transient--object :transient t)))

(defconst nvp-transient--dev-description "[NVP] Dev")
(defvar nvp-transient--done-deved nil)

(transient-define-infix nvp-transient-menu--debug ()
  "Toggle debug"
  :class 'transient-lisp-variable
  :variable 'transient--debug
  :reader (lambda (&rest _) (not transient--debug)))

;;;###autoload(autoload 'nvp-transient-menu "nvp-transient")
(transient-define-prefix nvp-transient-menu ()
  "Modify transients"
  [["Add"
    ("a" "Add dev bindings" nvp-transient-add-dev)]
   [ :if-non-nil nvp-transient--done-deved
     "Remove"
     ("r" "Remove bindings" nvp-transient-remove-dev)
     ("R" "Remove from all transients" nvp-transient-remove-all)]]
  ["Debug"
   ("d" "Toggle debug" nvp-transient-menu--debug)])

;;;###autoload
(defun nvp-transient-add-dev (transient &optional loc prefix)
  "Insert dev commands in front of TRANSIENT.
With \\[universal-argument] prompt for LOC and PREFIX key."
  (interactive
   (list (read--expression "Transient: ")
         (if current-prefix-arg (read--expression "Loc: " "(0)") '(0))
         (if current-prefix-arg (read-key-sequence "Prefix key: ") "/")))
  (let ((sufs (mapcar (lambda (e) (cons (concat prefix (car e)) (cdr e)))
                      nvp-transient--dev-suffixes)))
    (when (transient-insert-suffix transient loc
            `[,nvp-transient--dev-description ,@sufs])
      (cl-pushnew (list transient loc prefix)
                  nvp-transient--done-deved :test #'equal))))

(defun nvp-transient-remove-dev (transients)
  "Remove dev commands from TRANSIENT."
  (interactive (list (mapcar #'intern
                             (completing-read-multiple
                              "Transients: " nvp-transient--done-deved nil t))))
  (dolist (transient transients)
    (when-let ((val (get transient 'transient--layout)))
      (put transient 'transient--layout
           (seq-remove (lambda (e) (equal nvp-transient--dev-description
                                     (plist-get (aref e 2) :description)))
                       val))))
  (setq nvp-transient--done-deved
        (seq-filter (lambda (e) (not (seq-contains-p transients (car e) #'eq)))
                    nvp-transient--done-deved)))

(defun nvp-transient-remove-all ()
  "Remove dev commands from all modified transients."
  (interactive)
  (nvp-transient-remove-dev (mapcar #'car nvp-transient--done-deved)))

(provide 'nvp-transient)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-transient.el ends here
