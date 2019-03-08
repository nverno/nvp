;;; nvp-basic-auto.el --- basic autoloads -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-07 16:34:16>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created:  2 February 2019

;;; Commentary:
;; autos that will get called
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'nvp-macro)
  (require 'hydra))
(require 'nvp)

;; -------------------------------------------------------------------
;;; Hydras

;;;###autoload(autoload 'nvp-hydra-goto-line/goto-line "nvp-basic-auto")
(nvp-hydra-set-property 'nvp-hydra-goto-line)
(defhydra nvp-hydra-goto-line (goto-map) "line"
  ("g" goto-line "go")
  ("b" (push-mark (car mark-ring) nil 'activate) "mark to start")
  ("m" set-mark-command "mark" :bind nil)
  ("p" (set-mark-command 1) "pop" :bind nil)
  ("e" exchange-point-and-mark "exchange")
  ("q" nil "quit"))

;;; Yank / Pop
(declare-function helm-show-kill-ring "")
;;;###autoload(autoload 'nvp-hydra-yank-pop/yank "nvp-basic-auto")
;;;###autoload(autoload 'nvp-hydra-yank-pop/yank-pop "nvp-basic-auto")
(defhydra nvp-hydra-yank-pop ()
  "yank"
  ("C-y" yank nil)
  ("M-y" yank-pop nil)
  ("y" (yank-pop 1) "next")
  ("Y" (yank-pop -1) "prev")
  ;; ("l" helm-show-kill-ring "list" :color blue)
  )

;; -------------------------------------------------------------------
;;; Assorted
(nvp-declare "" nvp-move-previous-heading nvp-move-forward-heading
  nvp-local-header-regex)

;; FIXME: mark successive regions?
;;;###autoload
(defun nvp-mark-header-region ()
  "Mark current header region."
  (interactive)
  (let ((start (point)) beg)
    (condition-case nil
        (progn
          (forward-line 0)
          (or (looking-at (nvp-local-header-regex))
              (nvp-move-previous-heading 'error))
          ;; headers are known at this point
          (setq beg (point))
          (or (ignore-errors (nvp-move-forward-heading))
              (prog1 (goto-char (point-max))
                (message "Marked to end of buffer (no more headings)")))
          (push-mark (point) nil 'activate)
          (goto-char beg))
      (error
       (goto-char start)
       (user-error "Can't find header region to mark.")))))

;;;###autoload
(defun nvp-kill-emacs ()
  (interactive)
  (save-some-buffers 'no-ask)
  (kill-emacs))

(provide 'nvp-basic-auto)
;;; nvp-basic-auto.el ends here
