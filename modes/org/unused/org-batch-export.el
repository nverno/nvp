;;; org-batch-export.el --- Export org files to various formats from command-line.  -*- lexical-binding: t; -*-
;;; Code:
(add-to-list 'load-path (expand-file-name "~/.emacs.d/git/org-mode/lisp"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(defvar org-export-backends '(html latex texinfo))

(require 'org-loaddefs)

(defun org-export-file-to (backend src dest)
  (with-temp-buffer
    (insert-file-contents src)
    (org-export-to-file backend dest)))

(defvar file-suffixes
      '((html    . ".html")
        (latex   . ".tex")
        (pdf     . ".pdf")
        (texinfo . ".info")))

(defun org-export-dest (backend f)
  (concat (file-name-sans-extension f) (cdr (assoc backend file-suffixes))))

(defun batch-org-export-as (backend &optional noforce)
  "Run `org-export-as' with the given backend  on the files remaining on the 
command line.
Use this from the command line, with `-batch'; it won't work in
an interactive Emacs.  Each file is processed even if an error
occurred previously.  For example, invoke \"emacs -batch -f
batch-byte-compile $emacs/ ~/*.el\".  If NOFORCE is non-nil,
don't recompile a file that seems to be already up-to-date."
  ;; command-line-args-left is what is left of the command line, from
  ;; startup.el.
  (defvar command-line-args-left)       ;Avoid 'free variable' warning
  (if (not noninteractive)
      (error "`batch-org-export-as' is to be used only with -batch"))
  (let ((error nil))
    (while command-line-args-left
      (if (file-directory-p (expand-file-name (car command-line-args-left)))
          ;; Directory as argument.
          (let (source dest)
            (dolist (file (directory-files (car command-line-args-left)))
              (if (and (string-match emacs-lisp-file-regexp file)
                       (not (auto-save-file-name-p file))
                       (setq source
                             (expand-file-name file
                                               (car command-line-args-left)))
                       (setq dest (org-export-dest backend source))
                       (file-exists-p dest)
                       (file-newer-than-file-p source dest))
                  (if (null (org-export-file-to backend source dest))
                      (setq error t)))))
        ;; Specific file argument
        (let* ((source (car command-line-args-left))
               (dest (org-export-dest backend source)))
          (if (or (not noforce)
                  (or (not (file-exists-p dest))
                      (file-newer-than-file-p source dest)))
              (if (null (org-export-file-to backend (car command-line-args-left)
                                            dest))
                  (setq error t)))))
      (setq command-line-args-left (cdr command-line-args-left)))
    (kill-emacs (if error 1 0))))

(provide 'org-batch-export)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; org-batch-export.el ends here
