;;; nvp-elisp-abbrev.el --- create abbrevs -*- lexical-binding: t -*-

;;; Commentary:
;; Generates abbrevs for elisp functions defined in buffer.
;; By default, abbrevs are created from the first letters of
;; hyphenated words, ie. file-name-nondirectory => fnd.  The generated
;; abbrev table is made the local abbrev table with
;; `emacs-lisp-mode-abbrev-table' as its parent.
;;
;; TODO: generic methods for dynamic abbrevs
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'company-elisp))
(nvp:decls)
(require 'nvp-elisp)
(require 'nvp-abbrev-dynamic)

;; ------------------------------------------------------------
;;; Generate Abbrevs

;; (cl-defmethod nvp-abbrev-dynamic (&context (major-mode emacs-lisp-mode)
;;                                   &optional buffer-or-file append)
;;   (interactive
;;    (list (pcase (car current-prefix-arg)
;;            (4 (let ((lib (call-interactively #'locate-library)))
;;                 (setq lib (concat (file-name-sans-extension lib) ".el"))
;;                 (if (file-exists-p lib) lib
;;                   (concat lib ".gz"))))
;;            (16 (read-file-name "File to abbrev: "))
;;            (_ nil))
;;          (y-or-n-p "Append to current dynamic table? ")))
;;   (let ((buff (if (buffer-live-p buffer-or-file) buffer-or-file
;;                 (find-file-noselect buffer-or-file))))
;;     (set-buffer buff)
;;     (nvp-abbrev-dynamic-create append
;;                                :regexp nvp-lisp-abbrev-re
;;                                :enable-function #'nvp-elisp-abbrev-expand-fn-p
;;                                :parents (list emacs-lisp-mode-abbrev-table))))

;; ;; make abbrevs from current buffer and use as local abbrev table
;; ;;;###autoload
;; (defun nvp-elisp-abbrev-buffer (file)
;;   (interactive
;;    (list
;;     (pcase (car current-prefix-arg)
;;       (4 nil)
;;       (16 (let ((lib (call-interactively #'locate-library)))
;;             (setq lib (concat (file-name-sans-extension lib) ".el"))
;;             (if (file-exists-p lib) lib
;;               (concat lib ".gz"))))
;;       (_ (buffer-file-name)))))
;;   (let ((fns (or (and file (nvp-elisp--defs-from-file file))
;;                  (nvp-elisp--defs-from-buffer))))
;;     (when fns
;;       (when (bound-and-true-p nvp-abbrev-dynamic-table)
;;         (clear-abbrev-table nvp-abbrev-dynamic-table))
;;       (define-abbrev-table 'nvp-abbrev-dynamic-table
;;           (nvp-elisp--make-abbrevs :objects fns)
;;         :parents (list emacs-lisp-mode-abbrev-table)
;;         :regexp nvp-lisp-abbrev-re
;;         :enable-function 'nvp-elisp-abbrev-expand-fn-p)
;;       (setq-local local-abbrev-table nvp-abbrev-dynamic-table))))

(defun nvp-elisp-write-abbrev (file &optional verbose)
  (interactive
   (list (read-file-name
          "Write abbrev file: " default-directory "elisp-temp-abbrevs")))
  (let ((coding-system-for-write 'utf-8)
        (local-table nvp-abbrev-dynamic-table))
    (with-temp-buffer
      (set 'nvp-abbrev-dynamic-table local-table)
      (insert-abbrev-table-description (abbrev-table-name local-table) nil)
      (when (unencodable-char-position (point-min) (point-max) 'utf-8)
        (setq coding-system-for-write 'utf-8-emacs))
      (goto-char (point-min))
      (insert (format ";;-*-coding: %s;-*-\n" coding-system-for-write))
      (write-region nil nil file nil (and (not verbose) 0)))))

(provide 'nvp-elisp-abbrev)

;;; Local variables:
;;; lisp-indent-function: common-lisp-indent-function
;;; End:

;;; nvp-elisp-abbrev.el ends here
