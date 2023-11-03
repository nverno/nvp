;;; nvp-indicate.el --- indicators -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Indicate various things
;; - pulse regions with overlays
;; - temporarily change cursor color, eg. for hydra execution
;; - temporarily change modeline color with message
;; Commands to toggle indicators
;; - long lines
;; - trailing white space
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls
 :f (pulse-momentary-highlight-region pulse-momentary-highlight-one-line))

;; indicatation colors
(defvar nvp-indicate-colors
  '((failure . "#e31422")               ;red
    (success . "#44de11")))             ;green

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

;;;###autoload
(defun nvp-indicate-abort ()
  (interactive)
  (and (fboundp 'nvp-repeat-abort) (nvp-repeat-abort))
  (nvp-indicate-cursor-post))

;; ------------------------------------------------------------
;;; Temporarily change modeline color

;;;###autoload
(defun nvp-indicate-modeline (&optional msg type)
  "Flash success or TYPE in modeline with optional MSG."
  (let* ((power-p (memq 'powerline-active0 (face-list)))
         (pcolor (and power-p (face-background 'powerline-active0)))
         (orig-color (face-background 'mode-line))
         (temp-col (cdr (assoc (or type 'success) nvp-indicate-colors))))
    (when msg (message (concat (format "[%s] " (if type (upcase (symbol-name type))
                                                 "SUCCESS"))
                               msg)))
    (set-face-background 'mode-line temp-col)
    (and power-p (set-face-background 'powerline-active0 temp-col))
    (sit-for 3)
    (set-face-background 'mode-line orig-color)
    (and power-p (set-face-background 'powerline-active0 pcolor))))

(provide 'nvp-indicate)
;;; nvp-indicate.el ends here
