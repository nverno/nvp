;;; nvp-show.el --- Show/hide stuff -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp:decls)

(defvar-keymap nvp-repeat-show-format-map
  :repeat t
  "a" #'nvp-show-nonascii
  "l" #'nvp-show-long-lines
  "w" #'nvp-show-trailing-whitespace
  "d" #'delete-trailing-whitespace)

;;;###autoload
(defun nvp-show-nonascii (&optional remove all)
  "Toggle display of zero-width non-ascii characters in current buffer.
With prefix \\[universal-argument] REMOVE, remove displays.
With prefix \\[universal-argument] \\[universal-argument] ALL show all non-ascii chars."
  (interactive (list (eq 4 (prefix-numeric-value current-prefix-arg))
                     (eq 16 (prefix-numeric-value current-prefix-arg))))
  (if remove (remove-overlays (point-min) (point-max) 'nonascii t)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[[:nonascii:]]" nil t)
        (when (or all (zerop (char-width (char-after (match-beginning 0)))))
          (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
            (overlay-put ov 'display
                         (buttonize (format "(%d)" (char-after (match-beginning 0)))
                                    (let ((pos (match-beginning 0)))
                                      (lambda (_) (describe-char pos)))))
            (overlay-put ov 'face 'font-lock-warning-face)
            (overlay-put ov 'nonascii t)))))))

;;;###autoload
(defun nvp-show-long-lines (arg)
  "Toggle indication of long lines (length with prefix ARG, default 80)."
  (interactive "P")
  (nvp:toggled-if
      (let ((len (if arg (read-number "Length: ") 80))
            (font-lock-multiline t))
        (font-lock-add-keywords
         nil
         `((,(format "^[^\n]\\{%d\\}\\([^\n]+\\)$" len)
            (1 font-lock-warning-face t))))
        (font-lock-flush)
        (font-lock-ensure))
    (font-lock-refresh-defaults)))

;;;###autoload
(defun nvp-show-trailing-whitespace ()
  "Toggle indicatation of trailing whitespace."
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (font-lock-flush)
  (font-lock-ensure))

(provide 'nvp-show)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-show.el ends here
