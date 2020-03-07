;;; nvp-view-list.el --- view tabulated output -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;; XXXX: (3/7/20) not used
;;
;; View formatted results in temporary buffers:
;; - help-buffers
;; - tabulated lists
;; refs:
;; - timer-list.el
;; - view-list #<marker at 153343 in evil-common.el>
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp-decls)

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

(nvp-bindings "nvp-view-list" nil
  :with view
  ([return] . nvp-view-list-goto-entry)
  ("q"      . kill-this-buffer))


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

(provide 'nvp-view-list)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-view-list.el ends here
