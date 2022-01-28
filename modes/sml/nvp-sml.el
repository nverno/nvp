;;; nvp-sml.el --- smluts -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'sml-mode)
(declare-function tag-utils-tag-dir "tag-utils")
(declare-function smie-forward-sexp "smie")

(defvar nvp-sml-src-repo "https://smlnj-gforge.cs.uchicago.edu/svn")
(defvar nvp-sml-src-dir (expand-file-name "sml" (getenv "DEVEL")))

;; ------------------------------------------------------------
;;; Install / Tags

;; FIXME: source installed externally, remove tag-utils stuff
;; clone / update source, tag it
;; with two prefix arg, reinstall source if already have it
;; with single prefix, force retag
(defun nvp-sml-tag-source (arg &optional noretry)
  (interactive "P")
  (let ((have-src (file-exists-p nvp-sml-src-dir))
        (tags (expand-file-name "TAGS" nvp-sml-src-dir)))
    (if (and (not noretry)
             (or (not have-src) (eq '(16) arg)))
        ;; Get source / reinstall if have
        (progn
          (when have-src
            (delete-directory nvp-sml-src-dir))
          ;; (nvp-with-install-script nvp-sml--dir "install_sml_source" nil
          ;;   (nvp-sml-tag-source nil t))
          )
      ;; Otherwise, tag source / load tags table
      (when have-src
        (if (and (not (eq '(4) arg))
                 (file-exists-p tags))
            ;; without prefix arg, just load tags table
            (visit-tags-table tags)
          (tag-utils-tag-dir nvp-sml-src-dir))))))

;; ------------------------------------------------------------
;;; Interactive

;; FIXME: use newline-dwim
(nvp:newline nvp-sml-newline-dwim
  "Newline dwim for `sml-mode'"
  :comment-re (" *\\(?:(\\*\\|\\*\\)" . "\\*) *")
  :comment-start "* ")

;; FIXME: just use beginning/end of defun for movement/marking/folding

;; mark defuns successively when called repeatedly
(defun nvp-sml-mark-defun ()
  (interactive)
  (if (or (and (eq last-command this-command) (mark t))
          (and transient-mark-mode mark-active))
      (set-mark
       (save-excursion
         (goto-char (mark))
         (smie-forward-sexp 'halfsexp)
         (point)))
    (sml-mark-function)))

;;; Movement

(eval-when-compile
  (defmacro nvp-sml--search (regexp &optional back)
    `(let ((start (point)))
       (condition-case nil
           (progn
             (forward-line ,(if back -1 1))
             (,(if back 're-search-backward 're-search-forward) ,regexp)
             (beginning-of-line))
         (error (goto-char start))))))

(defun nvp-sml-previous-defun ()
  (interactive)
  (if (bolp)
      (nvp-sml--search "^fun" 'back)
    (sml-beginning-of-defun)))

(defun nvp-sml-next-defun ()
  (interactive)
  (if (bolp)
      (nvp-sml--search "^fun")
    (sml-beginning-of-defun)
    (smie-forward-sexp 'halfsexp)
    (nvp-sml--search "^fun")))

;; -------------------------------------------------------------------
;;; REPL

(declare-function sml-prog-proc-switch-to "sml-mode")

(defun nvp-sml-inf-newline ()
  (interactive)
  (end-of-line)
  (insert ";")
  (comint-send-input))

;; FIXME: replace with REPL interface
;; (nvp:repl-switch "sml" (:repl-mode 'inferior-sml-mode
;;                                    ))
(defvar nvp-sml--last-buffer nil)
(defun nvp-sml-switch-buffers ()
  (interactive)
  (if (and (eq major-mode 'inferior-sml-mode)
           nvp-sml--last-buffer)
      (switch-to-buffer-other-window nvp-sml--last-buffer)
    (setq nvp-sml--last-buffer (current-buffer))
    (sml-prog-proc-switch-to)))

(provide 'nvp-sml)
;;; nvp-sml.el ends here
