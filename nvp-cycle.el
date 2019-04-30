;;; nvp-cycle.el --- smart cycling -*- lexical-binding: t; -*-

;;; Commentary:

;; Cycle between options with single keystroke
;; refs: helm circular iterators: #<marker at 14458 in helm-lib.el>
;;
;; TODO:
;; - struct to hold options?
;; - set temporary overlay map
;; - after abbrev/snippet hook
;; Using overlays to mark regions like wgrep seems like it might work well.

;; Integrate:
;; - iedit restriction cycling

;; Useful functions:
;; - `cycle-spacing' (#<marker at 37959 in simple.el.gz>)
;;    examples in #<marker at 1838 in smartparens-ess.el>
;; - `just-one-space' (same; wrapper)

;; org-cycle: `org-pre-cycle-hook', `org-cycle-hook'

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'nvp)

;; TODO: bind cycle key to local overriding map when cycling in progress
(defvar-local nvp-cycling nil "Non-nil if cycle in progress")

(defvar-local nvp-cycle-index 0)
(defsubst nvp-cycle-next (actions &optional backward)
  (nth (mod (if backward (cl-decf nvp-cycle-index)
              (cl-incf nvp-cycle-index))
            (length actions))
       actions))

;;;###autoload
(defun nvp-cycle (&optional thing _backward)
  "Cycle through THING's actions.
Actions are list elements:
* function              - called with no args
* (function &rest args) - apply function to args
* otherwise             - just return element"
  (interactive)
  (let ((action (get thing 'nvp-cycle-actions)))
    (pcase action
      ((pred functionp) (funcall action))
      ((pred listp)
       (if (functionp (car action))
           (apply (car action) (cdr action))
         action))
      (_ action))))

;; (defun nvp-cycle-actions (actions)
;;   (let ((idx (or (get 'nvp-cycle-actions 'idx) 0)))
;;     (funcall (nth idx actions))
;;     (put 'nvp-cycle-actions 'idx (if (> (1+ idx) (length actions)) 0 (1+ idx)))))

(provide 'nvp-cycle)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-cycle.el ends here
