;;; nvp-show.el --- Show/hide stuff -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls)

(defvar-keymap nvp-repeat-show-format-map
  :repeat t
  "a" #'nvp-show-nonascii
  "l" #'nvp-show-formatting-dwim
  "w" #'nvp-show-trailing-whitespace
  "L" #'nvp-show-long-lines
  "d" #'delete-trailing-whitespace)

;;;###autoload
(defun nvp-show-nonascii (&optional show-all remove interactive)
  "Display zero-width non-ascii characters in buffer.
With prefix SHOW-ALL non-ascii chars.
With prefix <= 0 or REMOVE, remove overlays in buffer."
  (interactive (let ((arg (prefix-numeric-value current-prefix-arg)))
                 (list (> arg 0) (<= arg 0) t)))
  (if (or (and remove (not show-all))
          (and interactive (eq last-command this-command)
               (setq this-command 'nvp--toggle)))
      (progn (remove-overlays (point-min) (point-max) 'nonascii t)
             (and interactive (message "Removed overlays")))
    (let ((cnt 0))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "[[:nonascii:]]" nil t)
          (when (or show-all
                    (zerop (char-width (char-after (match-beginning 0)))))
            (cl-incf cnt)
            (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
              (overlay-put ov 'display
                           (buttonize
                            (format "(%d)" (char-after (match-beginning 0)))
                            (let ((pos (match-beginning 0)))
                              (lambda (_) (describe-char pos)))))
              (overlay-put ov 'face 'font-lock-warning-face)
              (overlay-put ov 'nonascii t)))))
      (if interactive
          (message "Found %d characters" cnt)
        cnt))))

;;;###autoload
(defun nvp-show-long-lines (&optional prompt enable interactive)
  "Toggle display of long lines and `fill-column' indicator.
With prefix, prompt for `fill-column'."
  (interactive (list current-prefix-arg nil t))
  (let ((active-p (bound-and-true-p display-fill-column-indicator-mode)))
    (when (or prompt (not (and enable active-p)))
      (cl-labels ((keywords (column)
                    `((,(format "^[^\n]\\{%d\\}\\([^\n]+\\)$" column)
                       (1 font-lock-warning-face t)))))
        (when prompt
          (and active-p
               (font-lock-remove-keywords nil (keywords fill-column)))
          (setq current-prefix-arg nil)
          (call-interactively #'set-fill-column))
        (let ((kws (keywords fill-column))
              (font-lock-multiline t))
          (if (or prompt enable (not active-p))
              (let ((font-lock-multiline t))
                (font-lock-add-keywords nil kws)
                (display-fill-column-indicator-mode 1))
            (font-lock-remove-keywords nil kws)
            (display-fill-column-indicator-mode -1))
          (and interactive (font-lock-update)))))))

;;;###autoload
(defun nvp-show-trailing-whitespace (&optional enable interactive)
  "Toggle indicatation of trailing whitespace."
  (interactive (list nil t))
  (unless (and enable show-trailing-whitespace)
    (setq show-trailing-whitespace (or enable (not show-trailing-whitespace)))
    (and interactive (font-lock-update))))

;;;###autoload
(defun nvp-show-formatting-dwim ()
  "Toggle formatting displays in buffer."
  (interactive)
  (nvp-show-trailing-whitespace)
  (nvp-show-long-lines current-prefix-arg)
  (font-lock-update))

(provide 'nvp-show)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-show.el ends here
