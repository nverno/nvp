;;; nvp-indicate.el --- indicators -*- lexical-binding: t; -*-
;;; Commentary:
;; Indicate various things
;; - pulse regions with overlays
;; - temporarily change modeline color with message
;;; Code:

(eval-when-compile (require 'nvp-macro))
(nvp:decls :p (pulse))

;; indicatation colors
(defvar nvp-indicate-colors
  '((failure . "#e31422")               ; red
    (success . "#44de11")))             ; green

;; store indicators
(defvar nvp-indicate--cache (make-hash-table))
(defun nvp-indicate-cache (key &optional value overwrite)
  (if value
      (if overwrite (puthash key value nvp-indicate--cache)
        (or (gethash key nvp-indicate--cache)
            (puthash key value nvp-indicate--cache)))
    (gethash key nvp-indicate--cache)))

;;;###autoload
(defun nvp-indicate-pulse-region-or-line (&optional beg end face)
  "Momentarily highlight line or region between BEG and END."
  (or face (setq face 'region))
  (if (and beg end)
      (pulse-momentary-highlight-region beg end face)
    (pulse-momentary-highlight-one-line (point) face)))

;;;###autoload
(defun nvp-indicate-modeline (&optional msg type)
  "Flash success or TYPE in modeline with optional MSG."
  (let* ((power-p (memq 'powerline-active0 (face-list)))
         (pcolor (and power-p (face-background 'powerline-active0)))
         (orig-color (face-background 'mode-line))
         (temp-col (cdr (assoc (or type 'success) nvp-indicate-colors))))
    (when msg
      (message "[%s] %s" (if type (upcase (symbol-name type)) "SUCCESS") msg))
    (set-face-background 'mode-line temp-col)
    (and power-p (set-face-background 'powerline-active0 temp-col))
    (sit-for 3)
    (set-face-background 'mode-line orig-color)
    (and power-p (set-face-background 'powerline-active0 pcolor))))

(provide 'nvp-indicate)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-indicate.el ends here
