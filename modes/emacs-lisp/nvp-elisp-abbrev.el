;;; nvp-elisp-abbrev.el --- create abbrevs -*- lexical-binding: t -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/elisp-utils
;; Last modified: <2019-03-15 06:57:48>
;; Created: 31 October 2016

;;; Commentary:

;; Generates abbrevs for elisp functions defined in buffer.
;; By default, abbrevs are created from the first letters of
;; hyphenated words, ie. file-name-nondirectory => fnd.  The generated
;; abbrev table is made the local abbrev table with
;; `emacs-lisp-mode-abbrev-table' as its parent.

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro)
  (require 'company-elisp)
  (defvar nvp-abbrev-dynamic-table)
  (nvp-local-vars))
(require 'nvp-elisp)
(require 'nvp-abbrev-dynamic)
(require 'nvp-parse)

;; -------------------------------------------------------------------
;;; Gather defines from file or buffer


(cl-defmethod nvp-parse-function-names
    (&context (major-mode emacs-lisp-mode) &optional buffer-or-file &rest args)
  (unless buffer-or-file (setq buffer-or-file (buffer-file-name)))
  (let* ((do-load (plist-get args :do-load))
         (bfs (nvp-elisp--file-forms buffer-or-file))
         (read-buff-p (not (or bfs do-load))))
    (or (and (not read-buff-p)
             (ignore-errors
               (nvp-elisp--defs-from-file buffer-or-file bfs do-load)))
        (with-current-buffer (if (buffer-live-p buffer-or-file)
                                 buffer-or-file
                               (find-file-noselect buffer-or-file))
          (nvp-elisp--defs-from-buffer)))))

;; try(not very hard) to gather buffer functions/macros at top level
;; from current buffer, optionally in region specified by BEG END
(defun nvp-elisp--defs-from-buffer (&optional beg end)
  (or end (setq end (point-max)))
  (save-excursion
    (let (forms form)
      (goto-char (or beg (point-min)))
      (ignore-errors
        (while (and (setq form (read (current-buffer)))
                    (< (point) end))
          (push (nvp-elisp--form-name form) forms)))
      (delq nil forms))))

;; return list of functions defined in buffer
(defun nvp-elisp--defs-from-file (file &optional bfs do-load)
  (unless bfs
    (and do-load (load-file file))
    (setq bfs (cl-assoc (regexp-quote file) load-history :test 'string-match-p)))
  (delq nil (mapcar (lambda (form) (nvp-elisp--form-name form)) bfs)))

;; return defun/macro part of form or nil
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Where-Defined.html
(defun nvp-elisp--form-name (form)
  (and (consp form)
       (cond
         ((memq (car form)
                '(defun defmacro defsubst cl-defun cl-defsubst cl-defmacro
                  declare-function t autoload cl-defmethod))
          (cadr form))
         ;; ((macrop (car form))
         ;;  (car form))
         (t nil))))

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
          "Write abbrev file: " (nvp-package-root) "elisp-temp-abbrevs")))
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
