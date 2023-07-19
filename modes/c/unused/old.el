;; -------------------------------------------------------------------
;;; Headers

;; jump to associated header, with arg create and/or update it as well
(defun nvp-c-jump-or-update-header (update)
  (interactive "P")
  (if update
      (call-interactively 'nvp-c-create-or-update-header)
    (condition-case nil
        (find-file-other-window (nvp:c--header-file-name))
      (error "oop"))))

(defun nvp-c-create-or-update-header (and-go)
  "Creates/updates header file with the function signatures in the current
source file."
  (interactive (list t))
  (let ((header (nvp:c--header-file-name))
        (sigs (nvp-c-function-signatures nil 'ignore-main 'ignore-static))
        (yas-wrap-around-region nil)
        (init t))
    (when (file-exists-p header)
      (setq init nil)
      (setq sigs
            ;; remove any signatures that are already found in the header file
            (cl-set-difference
             sigs
             (nvp-c-function-signatures header) :test 'string=)))
    (when (or init sigs)
      (with-current-buffer (find-file header)
        (and sigs
             (setq sigs (concat "\n" (mapconcat 'identity sigs ";\n") ";\n")))
        (if init
            ;; let ((yas-selected-text sigs))
            (yas-expand-snippet
             (yas-lookup-snippet "header" 'cc-mode)
             nil nil `((function-signatures ,sigs)))
          ;; insert at end, before final #endif
          (goto-char (point-max))
          (skip-chars-backward " \t\n\r\v") ;skip any trailing whitespace
          (forward-line -1)
          (insert sigs))))
    (when and-go
      (xref-push-marker-stack)
      (find-file header))))

;; add header guard
(defun nvp-c-add-guard ()
  (interactive)
  (let ((guard (concat
                (upcase (file-name-sans-extension
                         (file-name-nondirectory buffer-file-name)))
                "_H")))
    (save-excursion
      (goto-char (point-min))
      (unless (looking-at-p (format "#ifndef _%s" guard))
        (insert (format "#ifndef _%s\n#define _%s\n\n" guard guard))
        (goto-char (point-max))
        (insert (format "\n#endif /* _%s */" guard))))))
