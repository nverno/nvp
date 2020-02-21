;;; nvp-make-indent.el --- extra indentation in makefiles -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; XXX: not used currently -- needs some work
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-makefile)
(require 'nvp)
(nvp-decls)

;; indentation in blocks, eg.  #if, #define
(defvar nvp-makefile-indent-offset 2)

;; indent ifeq ... endif regions
;;;###autoload
(defun nvp-makefile-indent (&optional start finish)
  (or finish (setq finish (point-max)))
  (save-excursion
    (goto-char (or start (point-min)))
    (while (re-search-forward (car nvp-makefile-open/close) finish t)
      (let ((beg (match-beginning 0)) end)
        (when (re-search-forward
               (concat "^else\\|" (cadr nvp-makefile-open/close)) finish t)
          (setq end (nvp-point 'boll))
          (goto-char beg)
          (while (and (nvp-goto 'bonll)
                      (< (point) end)
                      (not (looking-at makefile-dependency-regex)))
            (delete-horizontal-space)
            (indent-to-column nvp-makefile-indent-offset))
          (goto-char end))))))

(provide 'nvp-make-indent)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-make-indent.el ends here
