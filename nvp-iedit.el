;;; nvp-iedit.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-03-09 06:37:39>
;; Created: 22 August 2018

;;; Commentary:

;; iedit extensions:
;; - toggle match restriction b/w global, defun, line
;; - expand regions during iedit

;; FIXME:
;; - Toggling region is pointless
;; - Generic cycling interface

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'iedit)
(nvp-declare "" nvp-mark-defun)

;; Add iedit bindings
(nvp-bind-keys iedit-mode-keymap
  ("C-=" . nvp-iedit-cycle-regions))

;; current level of iedit restriction: line, defun, buffer, region
(defvar-local nvp-iedit-restriction 'buffer)

(advice-add 'iedit-restrict-function
            :after (lambda (&rest _args) (setq nvp-iedit-restriction 'defun)))

(advice-add 'iedit-restrict-current-line
            :after (lambda (&rest _args) (setq nvp-iedit-restriction 'line)))

;; (add-hook 'iedit-mode-end-hook #'(lambda () (setq nvp-iedit-restriction 'buffer)))
;; (add-hook 'iedit-aborting-hook #'(lambda () (setq nvp-iedit-restriction nil)))

;;;###autoload
(defun nvp-iedit-dwim (arg)
  "With prefix ARG narrow to defun, with two prefix narrow to current line."
  (interactive "P")
  (if iedit-mode
      (progn
        (setq nvp-iedit-restriction 'buffer)
        (iedit-done))
    (iedit-mode)
    (pcase arg
      (`(16)
       (setq nvp-iedit-restriction 'line)
       (iedit-restrict-current-line))
      (`(4)
       (setq nvp-iedit-restriction 'defun)
       (iedit-restrict-function))
      (_))
    (nvp-msg "Toggle restrictions with \\[nvp-iedit-cycle-regions]"
      :delay 1 :keys t)))

(defun nvp-cycle-actions (actions)
  (let ((idx (or (get 'nvp-cycle-actions 'idx) 0)))
    (funcall (nth idx actions))
    (put 'nvp-cycle-actions 'idx (if (> (1+ idx) (length actions)) 0 (1+ idx)))))

;; allow expanding of restricted region when in `iedit-mode'
(defun nvp-iedit-cycle-regions ()
  "Cycle `iedit-mode' region restrictions."
  (interactive)
  (when iedit-mode
    (let ((occ-regexp (iedit-current-occurrence-string)))
      (if (region-active-p)
          (progn
            (setq nvp-iedit-restriction 'region)
            (iedit-restrict-region (region-beginning) (region-end)))
       (pcase nvp-iedit-restriction
         ((or 'line 'region)            ;restrict to defun
          (setq nvp-iedit-restriction 'defun)
          (save-mark-and-excursion
            (setq mark-active nil)
            (nvp-mark-defun)
            (let ((beg (region-beginning))
                  (end (region-end)))
              (goto-char beg)
              (while (and (< (point) end)
                          (condition-case nil
                              (iedit-add-occurrence-overlay occ-regexp
                                                            nil 'forward end)
                            (error (forward-word))))))))
         (`defun                        ;expand to buffer
             (setq nvp-iedit-restriction 'buffer)
             (iedit-make-occurrences-overlays occ-regexp (point-min) (point-max)))
         (_                             ;currently buffer => recycle
          (setq nvp-iedit-restriction 'line)
          (iedit-restrict-current-line)))))))

(provide 'nvp-iedit)
;;; nvp-iedit.el ends here
