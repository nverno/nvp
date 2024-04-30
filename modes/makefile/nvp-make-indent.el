;;; nvp-make-indent.el --- extra indentation in makefiles -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; XXX: not used currently -- needs some work
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-makefile)
(require 'nvp)
(nvp:decls)

;; indentation in blocks, eg.  #if, #define
(defvar nvp-makefile-indent-offset 2)

(defun nvp-makefile--indent-region (beg end)
  (goto-char beg)
  (while (and (nvp:goto 'bonll)
              (< (point) end)
              (not (looking-at makefile-dependency-regex)))
    (delete-horizontal-space)
    (indent-to-column nvp-makefile-indent-offset)))

;; indent ifeq ... endif regions
;;;###autoload
(defun nvp-makefile-indent (&optional start finish)
  (or finish (setq finish (point-max)))
  (save-excursion
    (goto-char (or start (point-min)))
    (while (re-search-forward (car nvp-makefile-open/close) finish t)
      (if (equal "define" (match-string 0))
          (when (re-search-forward "^endef" nil t)
            (goto-char (line-beginning-position 2)))
        (let ((beg (match-beginning 0)) done end)
          (while (and (re-search-forward
                       (concat "^else\\|" (cadr nvp-makefile-open/close)) finish t)
                      (not done))
            (setq done (not (string= "else" (match-string 0))))
            (setq end (nvp:point 'boll))
            (nvp-makefile--indent-region beg end)
            (goto-char end)
            (goto-char (line-end-position))
            (or done (setq beg (point)))))))))

(provide 'nvp-make-indent)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-make-indent.el ends here
