;;; exenv.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls)
(defvar exenv-dir "~/.exenv")

;; -------------------------------------------------------------------
;;; List

(eval-when-compile
  (defvar tabulated-list-format)
  (defvar tabulated-list-entries))
(declare-function tabulated-list-init-header "tabulated-list")
(declare-function tabulated-list-print "tabulated-list")
(declare-function tabulated-list-get-entry "tabulated-list")

(defun exenv-available-installs ()
  "Exenv list available installs as tabulated list."
  (interactive)
  (let ((installs
         (mapcar #'(lambda (s)
                     (let ((str (string-trim s)))
                       (list str (vector str))))
                 (cdr (process-lines "exenv" "install" "--list"))))
        (buff (get-buffer-create "*exenv*")))
    (with-current-buffer buff
      (setq tabulated-list-format [("Build" 20 nil)])
      (setq tabulated-list-entries installs)
      (exenv-mode)
      (tabulated-list-print)
      (pop-to-buffer (current-buffer)))))

(defun exenv-install (id)
  (interactive (list (aref (tabulated-list-get-entry) 0)))
  (nvp:with-process-log
    (start-process-shell-command
     "exenv" "*exenv*" (concat "exenv install " id))
    :on-error (pop-to-buffer (current-buffer))
    :on-success (view-mode))
  (display-buffer "*exenv*"))

(defvar exenv-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "RET") 'exenv-install)
    km))

(define-derived-mode exenv-mode tabulated-list-mode
  "Exenv"
  "Commands: 
\\{exenv-mode-map}"
  (tabulated-list-init-header))

(provide 'exenv)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; exenv.el ends here
