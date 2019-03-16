;;; nvp-log.el ---  -*- lexical-binding: t; -*-

;; Last modified: <2019-03-15 19:15:36>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created:  2 November 2016

;;; Commentary:
;; FIXME: get rid of this, find better strategy
;; basic logging mode
;;; Code:
(eval-when-compile
  (require 'nvp-macro))

(defvar nvp-log-buffer "*nvp-log*")

;;;###autoload
(defun nvp-log (text &optional buffer-name &rest args)
  (let ((buffer-name (or buffer-name
                         (bound-and-true-p nvp-log-buffer)))
        deactivate-mark)
    (with-current-buffer (get-buffer-create buffer-name)
      (nvp-log-mode)
      (goto-char (point-max))
      (insert-before-markers
       (apply 'format 
              (replace-regexp-in-string
               "\n+" "\n" (concat text "\n"))
              args)))))

;; ------------------------------------------------------------
;;; Mode

(require 'compile)

(defvar nvp-log-font-lock
  (eval-when-compile
    (let ((gstat (nvp-re-opt '("finished" "deleted" "open" "success")))
          (wstat (nvp-re-opt '("warning")))
          (rstat (nvp-re-opt
                  '("exited abnormally" "failed" "error" "signal-description"
                    "connection broken")))
          (gproc (nvp-re-opt '("run" "open" "listen" "connect")))
          (rproc (nvp-re-opt '("stop" "exit" "signal" "closed" "failed"))))
      `(("`\\([^\n']+\\)'" (1 font-lock-constant-face))
        (,gstat (1 'compilation-info))
        (,wstat (1 'compilation-warning))
        (,rstat (1 'compilation-line-number))
        (,gproc (1 'compilation-info))
        (,rproc (1 'compilation-line-number))))))

;;;###autoload
(define-derived-mode nvp-log-mode fundamental-mode "Log"
  (setq-local font-lock-defaults
              '(nvp-log-font-lock nil t nil nil)))

;; -------------------------------------------------------------------
;;; View list - simple tabulated display
;; #<marker at 153343 in evil-common.el>

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

(nvp-bindings-with-view "nvp-view-list" nil
  ([return] . nvp-view-list-goto-entry)
  ("q"      . kill-this-buffer))

(provide 'nvp-log)
;;; nvp-log.el ends here
