;;; nvp-cycle.el --- smart cycling -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Cycle between options with repeated keypress
;;
;; TODO:
;; - after abbrev/snippet hook
;; Using overlays to mark regions like wgrep seems like it might work well.

;; Integrate:
;; - iedit restriction cycling

;; Useful functions:
;; - `cycle-spacing' (#<marker at 37959 in simple.el.gz>)
;;    examples in #<marker at 1838 in smartparens-ess.el>
;; - `just-one-space' (same; wrapper)
;;
;; org-cycle: `org-pre-cycle-hook', `org-cycle-hook'
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp-decls)

(defvar-local nvp-cycling nil "Non-nil if cycle in progress")

;; see `helm-iter-circular'
(defun nvp-cycle-iter (seq)
  "Circular iterator over SEQ."
  (let ((lis seq))
    (lambda ()
      (let ((elm (car lis)))
        (setq lis (pcase lis (`(,_ . ,ll) (or ll seq))))
        elm))))

(defun nvp-cycle-next (iterator)
  "Next element of ITERATOR."
  (and iterator (funcall iterator)))

(defun nvp-cycle-elem (elem &rest args)
  (pcase elem
    ((pred functionp) (funcall-interactively elem args))
    ((pred stringp) (insert elem))
    (_ (if (and (listp elem) (functionp (car elem)))
           (apply (car elem) (cdr elem))
         (user-error "unhandled cycle item: %S" elem)))))

(defun nvp-cycle-stop ()
  (setq nvp-cycling nil))

(defun nvp-cycle-step (iterator &rest args)
  (let ((iter iterator))
    (lambda ()
      (interactive)
      (undo)
      (undo-boundary)
      (let ((elem (nvp-cycle-next iter)))
        (nvp-cycle-elem elem args)))))

;;;###autoload
(defun nvp-cycle (key seq &rest args)
  (let* ((iter (nvp-cycle-iter seq))
         (fn (nvp-cycle-step iter args)))
    (nvp-cycle-elem (nvp-cycle-next iter) args)
    (setq nvp-cycling t)
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map key fn)
       map)
     t #'nvp-cycle-stop)))

(provide 'nvp-cycle)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-cycle.el ends here
