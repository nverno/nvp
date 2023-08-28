
;; -------------------------------------------------------------------
;;; Insert / Toggle

;; rust mode functions:
;; rust-in-comment-paragraph
;; rust-in-macro
;; rust-in-str-or-cmnt
;; rust-beginning-or-defun
;; rust-end-of-defun

;; toggle public visibility of function at point
(defun nvp-rust-toggle-pub ()
  (interactive)
  (save-excursion
    (rust-beginning-of-defun)
    (if (and (looking-at-p "pub ")
             (message "pub off"))
        (delete-char 4)
      (insert "pub ")
      (message "pub on"))))

;; toggle mutability of variable at point
(defun nvp-rust-toggle-mut ()
  (interactive)
  (save-excursion
    (racer-find-definition)
    (back-to-indentation)
    (forward-char 4)
    (if (and (looking-at-p "mut ")
             (message "mutability off"))
        (delete-char 4)
      (insert "mut ")
      (message "mutability on"))))

;;; Enums / Struct

(eval-when-compile
  ;; do body business at item location
  (defmacro nvp-rust-at-definition-of (item &rest body)
    (declare (indent defun))
    `(save-excursion
       (with-current-buffer (find-file-noselect
                             (get-text-property 0 'file ,item))
         (goto-char (point-min))
         (forward-line (1- (get-text-property 0 'line ,item)))
         (forward-char (get-text-property 0 'col ,item))
         ,@body)))

  ;; collect stuff between beginning/end of rust def at point
  (defmacro nvp-rust-collect-fields (regexp)
    (declare (indent defun))
    `(save-excursion
       (rust-beginning-of-defun)
       (let ((end (save-excursion
                    (rust-end-of-defun)
                    (point)))
             (case-fold-search)
             opts)
         (while (< (point) end)
           (and (looking-at ,regexp)
                (push (match-string-no-properties 1) opts))
           (forward-line 1))
         opts))))
