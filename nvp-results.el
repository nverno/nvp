;;; nvp-results.el --- viewing results -*- lexical-binding: t; -*-

;;; Commentary:

;; View formatted results in temporary buffers:
;; - help-buffers
;; - tabulated lists
;; refs:
;; - timer-list.el
;; - view-list #<marker at 153343 in evil-common.el>

;;; Code:
(eval-when-compile
  (require 'nvp-macro))
(require 'nvp)
(nvp-auto "nvp-util" nvp-s-repeat nvp-s-center)
(nvp-decls)

;; -------------------------------------------------------------------
;;; Pretty printing

;; ielm's nice formatting: #<marker at 14912 in ielm.el.gz>

;; print centered TITLE
;;;###autoload
(defun nvp-results-title (title &optional width char)
  (or width (setq width 85))
  (or char (setq char "~"))
  (princ (format "\n%s\n%s\n\n" (nvp-s-center (- width (length title)) title)
                 (nvp-s-repeat width "~"))))

(defun nvp-pp-variable-to-string (variable)
  (let ((print-escape-newlines t)
        (print-quoted t)
        (var (if (symbolp variable) (eval variable)
               variable)))
    (pcase var
      ((pred hash-table-p)
       (nvp-pp-hash var))
      ;; TODO: structs/classes
      (_
       ;; (cl-prin1-to-string var)
       (pp-to-string var)))))

(defun nvp-pp-hash (hash)
  (with-temp-buffer
    (lisp-mode-variables nil)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (maphash (lambda (key val)
               (pp key)
               (princ " => ")
               (pp val)
               (terpri))
             hash)
    (buffer-string)))

;; -------------------------------------------------------------------
;;; View list - simple tabulated display

(defvar-local nvp-view-list-select-action ())
(put 'nvp-view-list-select-action 'permanent-local t)

(defun nvp-view-list-goto-entry ()
  (interactive)
  (when (and nvp-view-list-select-action
             (not (eobp)))
    (let* ((line (line-number-at-pos (point)))
           (entry (elt tabulated-list-entries (1- line))))
      (funcall nvp-view-list-select-action (nth 1 entry)))))

;;;###autoload
(define-derived-mode nvp-view-list-mode tabulated-list-mode
  "Simple list view."
  (tabulated-list-init-header)
  (tabulated-list-print))

(nvp-bindings-with-view "nvp-view-list" nil
  ([return] . nvp-view-list-goto-entry)
  ("q"      . kill-this-buffer))


;; -------------------------------------------------------------------
;;; Output

(cl-defmacro nvp-with-results-buffer (&optional buffer-or-name &rest body
                                                &key font-lock &allow-other-keys)
  "Do BODY in temp BUFFER-OR-NAME as with `with-temp-buffer-window'.
Make the temp buffer scrollable, in `view-mode' and kill when finished."
  (declare (indent defun) (debug (sexp &rest form)))
  (while (keywordp (car body))
    (setq body (cdr (cdr body))))
  `(let (other-window-scroll-buffer)
     (nvp-window-configuration-save)
     (with-temp-buffer-window
      ,(or buffer-or-name '(help-buffer))
      t
      nil
      (with-current-buffer standard-output
        (setq other-window-scroll-buffer (current-buffer))
        ,@body
        ,@(when font-lock '((font-lock-mode) (font-lock-ensure)))
        (hl-line-mode)
        (view-mode-enter nil #'nvp-window-configuration-restore)))))

;; evil-with-view-list: #<marker at 154076 in evil-common.el>
(cl-defmacro nvp-with-view-list (&key
                                 name           ;buffer name
                                 mode-name      ;mode-line name
                                 format         ;`tabulated-list-format'
                                 entries        ;`tabulated-list-entries'
                                 select-action) ;function applied to row
  "View buffer in `tabulated-list-mode'."
  (declare (indent defun) (debug t))
  (let ((action (make-symbol "action")))
    `(progn
       (let ((,action ,select-action)
             (bufname (concat "*" ,name "*"))
             (inhibit-read-only t))
         (and (get-buffer bufname)
              (kill-buffer bufname))
         (let ((buf (get-buffer-create bufname)))
           (with-current-buffer buf
             (setq tabulated-list-format ,format
                   tabulated-list-entries ,entries
                   action ,action)
             (nvp-view-list-mode)        ;inits lists
             (setq mode-name ,mode-name))
           (pop-to-buffer buff))))))

(provide 'nvp-results)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-results.el ends here
