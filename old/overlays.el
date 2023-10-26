;; -------------------------------------------------------------------  -*- lexical-binding: t; -*-
;;; Overlays

;; remove NAME overlays from region BEG END
(defun nvp-remove-overlays (beg end name)
  (dolist (ov (overlays-in beg end))
    (when (overlay-get ov name)
      (delete-overlay ov))))

;; make NAME overlay from BEG to END current buffer
;; non-nil FRONT-ADVANCE => text *not* in overlay
;; non-nil REAR-ADVANCE => text *is* in overlay
;; (defun nvp-make-overlay (beg end name &optional front-advance rear-advance)
;;   (let ((o (make-overlay beg end nil front-advance rear-advance)))
;;     (overlay-put o name)
;;     o))

;; collect NAME overlays in buffer
(defun nvp-file-overlays (name)
  (save-restriction
    (widen)
    (let (res)
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (overlay-get ov name)
          (push ov res)))
      (nreverse res))))
