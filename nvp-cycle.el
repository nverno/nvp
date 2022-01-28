;;; nvp-cycle.el --- smart cycling -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Cycle between options with repeated keypress
;;
;; TODO:
;; - after abbrev/snippet hook
;; Using overlays to mark regions like wgrep seems like it might work well.
;;
;; Integrate:
;; - iedit restriction cycling
;;
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
(nvp:decls)

;;; XXX: isn't used
;; (defvar-local nvp-cycling nil "Non-nil if cycle in progress")

;; (defsubst nvp-cycle-stop ()
;;   (setq nvp-cycling nil))

;; see `helm-iter-list', `helm-iter-circular'
;; #<marker at 16718 in helm-lib.el>
;; also `helm-cycle-resume' #<marker at 115465 in helm.el> to resume
;; iteration from a specific position -- currently this doesn't support that
(defun nvp-cycle-iter (seq)
  "Circular iterator over SEQ."
  (let ((lis seq))
    (lambda ()
      (let ((elm (car lis)))
        (setq lis (pcase lis (`(,_ . ,ll) (or ll seq))))
        elm))))

(eval-when-compile
  (defsubst nvp-cycle-next (iterator)
    ;; Next element of ITERATOR.
    (and iterator (funcall iterator))))

(defun nvp-cycle-elem (elem &rest args)
  (pcase elem
    ((pred functionp) (funcall-interactively elem args))
    ((pred stringp) (insert elem))
    (_ (if (and (listp elem) (functionp (car elem)))
           (apply (car elem) (cdr elem))
         (user-error "unhandled cycle item: %S" elem)))))

(defun nvp-cycle-step (iterator &rest args)
  (let ((iter iterator))
    (lambda ()
      (interactive)
      (undo)
      (undo-boundary)
      (let ((elem (nvp-cycle-next iter)))
        (nvp-cycle-elem elem args)))))

(defun nvp-cycle-insert (iterator)
  "Iterator that inserts elements, deleting back to starting point each iteration."
  (let ((iter iterator)
        (beg (point)))
    (lambda ()
      (interactive)
      (with-silent-modifications        ; try not to pollute undo history
        (when (> (point) beg)
          (delete-region beg (point)))
        (insert (nvp-cycle-next iter))))))

;;;###autoload
(cl-defun nvp-cycle (key seq &key keymap
                         (iter-fn #'nvp-cycle-insert) 
                         exit-fn)
  "Cycle SEQ with KEYpresses indefinetly.
ITER-FN is called to handle each element, called with iterator.
KEYMAP if non-nil, is used when setting up a transient map with KEY bound to
the iterator function.
EXIT-FN is called when exiting transient map."
  (let* ((iter (nvp-cycle-iter seq))
         (fn (funcall iter-fn iter))
         (map (or keymap (make-sparse-keymap))))
    (define-key map key fn)
    (funcall-interactively fn)          ; call once before installing map
    (set-transient-map map t exit-fn)))
(put 'nvp-cycle 'lisp-indent-function 2)

(provide 'nvp-cycle)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-cycle.el ends here
