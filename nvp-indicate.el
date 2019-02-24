;;; nvp-indicate.el --- indicators -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-23 23:57:35>
;; Created:  2 November 2016

;;; Commentary:

;; Indicate various things
;; - pulse regions with overlays
;; - temporarily change cursor color, eg. for hydra execution
;; - temporarily change modeline color with message
;; Commands to toggle indicators
;; - long lines
;; - trailing white space

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(nvp-declare "pulse"
  pulse-momentary-highlight-region pulse-momentary-highlight-one-line)

;; store indicators
(defvar nvp-indicate--cache (make-hash-table))

(defun nvp-indicate-cache (key &optional value overwrite)
  (if value
      (if overwrite (puthash key value nvp-indicate--cache)
        (or (gethash key nvp-indicate--cache)
            (puthash key value nvp-indicate--cache)))
    (gethash key nvp-indicate--cache)))

;; -------------------------------------------------------------------
;;; Temporarily highlight

;;;###autoload
(defun nvp-indicate-pulse-region-or-line (&optional beg end face)
  "Momentarily highlight line or region between BEG and END."
  (or face (setq face 'next-error))
  (if (and beg end)
      (pulse-momentary-highlight-region beg end face)
    (pulse-momentary-highlight-one-line (point) face)))

;; -------------------------------------------------------------------
;;; Cursor

;;; pre/post change cursor color, eg. for hydra or transient state
;;;###autoload
(defun nvp-indicate-cursor-pre ()
  (nvp-indicate-cache 'cursor-color (face-attribute 'cursor :background))
  (set-cursor-color "#e31422"))

;;;###autoload
(defun nvp-indicate-cursor-post ()
  (set-cursor-color (nvp-indicate-cache 'cursor-color)))

;; ------------------------------------------------------------
;;; Temporarily change modeline color

;; make modeline green for a sec
;;;###autoload
(defun nvp-indicate-modeline-success (&optional msg)
  "Flash success in modeline with optional MSG."
  (let* ((power-p (memq 'powerline-active0 (face-list)))
         (pcolor (and power-p (face-background 'powerline-active0)))
         (color (face-background 'mode-line)))
    (when msg (message (concat "[SUCCESS]" msg)))
    (set-face-background 'mode-line "#44de11")
    (and power-p (set-face-background 'powerline-active0 "#44de11"))
    (sit-for 1.5)
    (set-face-background 'mode-line color)
    (and power-p (set-face-background 'powerline-active0 pcolor))
    ;; (add-hook 'post-command-hook #'nvp-indicate-modeline-revert nil t)
    ))

;; (defun nvp-indicate-modeline-revert (&optional color)
;;   (remove-hook 'post-command-hook #'nvp-indicate-modeline-revert t)
;;   (set-face-background 'mode-line color))

;; ------------------------------------------------------------
;;; Toggle font-locking for long lines

;;;###autoload
(defun nvp-indicate-long-lines (arg)
  "Toggle indication of long lines (length with prefix ARG, default 80)."
  (interactive "P")
  (nvp-toggled-if (font-lock-refresh-defaults)
    (let ((len (if arg (read-number "Length: ") 80)))
      (font-lock-add-keywords
       nil `((,(format "^[^\n]\\{%d\\}\\(.*\\)$" len) 1 font-lock-warning-face t)))
      (font-lock-flush)
      (font-lock-ensure))))

;;;###autoload
(defun nvp-indicate-trailing-whitespace ()
  "Toggle indicatation of trailing whitespace."
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (font-lock-flush)
  (font-lock-ensure)
  ;; when show-trailing-whitespace
  (nvp-use-transient-bindings (("d" . delete-trailing-whitespace))))

(provide 'nvp-indicate)
;;; nvp-indicate.el ends here
