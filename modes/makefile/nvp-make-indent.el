;;; nvp-make-indent.el --- extra indentation in makefiles -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Indents regions between `nvp-makefile-open/close'. In a sequence like
;;
;;    ifeq <region> else <region> <endif>
;;
;; Code in <region>s is indented `nvp-makefile-indent-offset' unless the line
;; starts with a TAB, in which nothing is done.
;;
;; Note(5/2/24): Currently, define .. endef regions are ignored.
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-makefile)
(nvp:decls)

;; indentation in blocks, eg.  #if, #define
(defvar nvp-makefile-indent-offset 2)

(defun nvp-makefile--indent-region (beg end)
  (goto-char beg)
  (while (and (nvp:goto 'bonll)
              (< (point) end)
              (not (eq ?	(char-after)))
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
