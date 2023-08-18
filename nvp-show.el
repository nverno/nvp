;;; nvp-show.el --- Show/hide stuff -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp:decls)

;;;###autoload
(defun nvp-show-nonascii (arg)
  "Toggle display of nonascii characters in current buffer.
With prefix ARG, remove displays."
  (interactive "P")
  (if arg (remove-overlays (point-min) (point-max) 'nonascii t)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[[:nonascii:]]" nil t)
        (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put ov 'display
                       (buttonize (format "(%d)" (char-after (match-beginning 0)))
                                  (let ((pos (match-beginning 0)))
                                    (lambda (_) (describe-char pos)))))
          (overlay-put ov 'face 'font-lock-warning-face)
          (overlay-put ov 'nonascii t))))))

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
    (font-lock-refresh-defaults))
  (nvp-repeat-command))

;;;###autoload
(defun nvp-show-trailing-whitespace ()
  "Toggle indicatation of trailing whitespace."
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (font-lock-flush)
  (font-lock-ensure)
  ;; when show-trailing-whitespace
  (nvp-repeat-command nil nil '(("d" 'delete-trailing-whitespace))))


(provide 'nvp-show)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-show.el ends here
