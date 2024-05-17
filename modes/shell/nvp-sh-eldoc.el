;;; nvp-sh-eldoc.el --- eldoc for bash/sh  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Notes:
;; - help strings are cached
;; - bash builtins run synchronously
;; - others run async, so help will show up after moving point
;;   in region more than once
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-shell-common)
(require 'nvp-sh-help)


(defvar nvp-sh-eldoc-cache (make-hash-table :test 'equal))

;; Doc strings for bash builtins
(defun nvp-sh-eldoc-builtin-string (cmd)
  (or (gethash cmd nvp-sh-eldoc-cache)
      (let ((str (nvp-sh-bash-builtin-help-sync cmd 'synopsis)))
        ;; remove 'cmd: ' and trailing newline
        (puthash
         cmd (substring str (+ 2 (length cmd)) (1- (length str)))
         nvp-sh-eldoc-cache))))

;; Get synopsis from man output asynchronously and cache it
(defun nvp-sh-eldoc--man (cmd)
  (nvp-sh:with-man-help cmd nil
    (goto-char (point-min))
    (ignore-errors
      (when (search-forward "SYNOPSIS")
        (while (not (looking-at-p (concat "\\([ \t]+" cmd "\\|^[^ \t]\\)")))
          (forward-line))
        (skip-chars-forward " \t")
        (puthash                        ; put result in cache
         cmd (and (looking-at "[^ \t]+[ \t]+\\([^\n]+\\)")
                  (match-string 1))
         nvp-sh-eldoc-cache)
        (erase-buffer)))))

;;;###autoload
(defun nvp-sh-eldoc-function (callback &rest _)
  "Return eldoc string for bash functions (builtins and those avaliable
 from \"man %s\")."
  (when-let* ((func (nvp-sh-current-command))
              (sig (nvp-sh:with-bash/man func
                       (nvp-sh-eldoc-builtin-string func) ; synchronously
                     ;; get doc string from man async
                     (or (gethash func nvp-sh-eldoc-cache)
                         (ignore (nvp-sh-eldoc--man func))))))
    (funcall callback sig :thing func :face 'font-lock-function-name-face)))

(provide 'nvp-sh-eldoc)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-sh-eldoc.el ends here
