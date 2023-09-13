;;; nvp-transient.el --- transient dev -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Add/remove dev commands from transient menus
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'transient)

(transient-define-suffix nvp-transient--args ()
  "Show current infix args."
  :description "Infix args"
  (interactive)
  (message "%S" (transient-args transient-current-command)))

(transient-define-suffix nvp-transient--value ()
  "Show current value."
  :description "Current Value"
  (interactive)
  (message "%S" (transient-get-value)))

(transient-define-suffix nvp-transient--object ()
  "Show object."
  :description "Object"
  (interactive)
  (message "%S" transient-current-prefix))

(defvar nvp-transient--dev-suffixes
  '(("a" nvp-transient--args :transient t)
    ("v" nvp-transient--value :transient t)
    ("o" nvp-transient--object :transient t)))

(defvar nvp-transient--dev-description "Nvp")
(defvar nvp-transient--done-deved nil)

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
      (cl-pushnew (list transient loc prefix) nvp-transient--done-deved :test #'equal))))

(defun nvp-transient-remove-dev (transient)
  "Remove dev commands from TRANSIENT."
  (interactive (list (read--expression "Transient: ")))
  (when-let ((val (get transient 'transient--layout)))
    (put transient 'transient--layout
         (seq-remove
          (lambda (e) (equal nvp-transient--dev-description
                        (plist-get (aref e 2) :description)))
          val)))
  (setq nvp-transient--done-deved
        (cl-delete transient nvp-transient--done-deved :test (lambda (x e) (eq x (car e))))))

(defun nvp-transient-remove-all ()
  "Remove dev commands from all modified transients."
  (interactive)
  (dolist (x nvp-transient--done-deved)
    (nvp-transient-remove-dev (car x))))

(provide 'nvp-transient)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-transient.el ends here
