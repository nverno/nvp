(autoload 'nvp-yaml-indent-line "nvp-yaml-indent")

;; if non-nil then something was inserted, so
;; push-mark at end of insertion
(defvar-local yas-yaml-inserted nil)

;; If program isnt defined, then insert INSERTION in SECTION
;; Also, set location so when snippet is done we can pop back
(defun yas-yaml-insert-decl (program &optional section insertion)
  (let ((case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      (when (and (search-forward (or section "global:") nil t)
                 (save-excursion
                   (not (re-search-forward (concat program "\\s-*=") nil t))))
        (insert "\n")
        (nvp-yaml-indent-line)
        (insert (format "- %s" (or insertion (concat (upcase program) "="))))
        (setq yas-yaml-inserted (point))
        nil))))

;; push mark
(defun yas-yaml-post-insert (&optional fn)
  (when yas-yaml-inserted
    ;; push current location then inserted location
    ;; to pop there then back
    (push-mark)
    (push-mark yas-yaml-inserted)
    (setq yas-yaml-inserted nil)
    (when fn
      (funcall fn))))

;; return column of current indent
(defun yas-yaml-indent-column ()
  (save-excursion
    (back-to-indentation)
    (current-column)))
