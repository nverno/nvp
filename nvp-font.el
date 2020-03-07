;;; nvp-font.el --- add/toggle additional font-locking -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Utils to add additional font-locking
;;
;; - https://github.com/ShingoFukuyama/ov.el
;;
;; TODO:
;; - update a mode's additions w/o creating entire new entry
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)

;; -------------------------------------------------------------------
;;; Fontify things in quoted regions

;; Non-nil if point is inside a CHAR quoted region
(defsubst nvp-font-lock-quoted-p (&optional char)
  (eq (nth 3 (syntax-ppss)) (or char ?\")))

;; add fontification to REGEX up to LIMIT in quoted area by CHAR
;; (default double-quotes)
;;;###autoload
(defun nvp-fontify-quoted-1 (regex char limit)
  (let (res)
    (while (and (setq res (re-search-forward regex limit 'move))
                (not (nvp-font-lock-quoted-p char))))
    res))

(cl-defmacro nvp-fontify-quoted (&rest forms &key char &allow-other-keys)
  "Fontify elements in quoted regions."
  (declare (indent defun))
  (while (keywordp (car forms))
    (setq forms (cdr (cdr forms))))
  (unless char (setq char ?\"))
  (macroexp-progn
   (cl-loop for (regex font) in forms
      collect `(cons ,(apply-partially #'nvp-fontify-quoted-1 regex char)
                     '(,font)))))

;; -------------------------------------------------------------------
;;; Create font-lock additions

(defmacro nvp-font-lock-keywords (&rest forms)
  "Create list of font-lock additions.
Each element of FORMS is a list ([:quoted char] regex font-spec)."
  (declare (indent defun) (debug t))
  `(append
    ,@(cl-loop for form in forms
         as quoted = (plist-get form :quoted)
         as splice = (plist-get form :splice)
         when (consp form)
         do (while (keywordp (car form))
              (if (memq (car form) '(:splice)) ; ill-formed plist keys
                  (setq form (cdr form))
                (setq form (cdr (cdr form)))))
         if (and splice (consp form))
         collect (car form)
         else
         ;; collect `,(car form)
         ;; else
         if quoted
         collect `(list (nvp-fontify-quoted :char ,quoted ,form))
         else
         collect (if (consp form)
                     `(list (cons ,(car form) ',(cdr form)))
                   `(list ,form)))))

(defmacro nvp-font-lock-add-defaults (mode &rest forms)
  "Add font-lock additions to MODE."
  (declare (indent defun))
  (macroexp-let2 nil fonts `(progn (nvp-font-lock-keywords ,@forms))
   `(progn
      (cl-pushnew (cons ,mode ,fonts) nvp-mode-font-additions :test #'equal)
      (font-lock-add-keywords ,mode ,fonts))))

;; -------------------------------------------------------------------
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

(provide 'nvp-font)
;;; nvp-font.el ends here
