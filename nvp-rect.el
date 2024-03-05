;;; nvp-rect.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'transient)
(require 'rect)

(defun nvp-rect--exit-hook ()
  (remove-hook 'transient-exit-hook #'nvp-rect--exit-hook)
  (deactivate-mark))

(defun nvp-rect-menu--reset ()
  (interactive)
  (if (region-active-p)
      (deactivate-mark)
    (rectangle-mark-mode 1)))

;; Exchange point and mark.
(defun nvp-rect-ex-point-mark ()
  (interactive)
  (if rectangle-mark-mode
      (rectangle-exchange-point-and-mark)
    (let ((mk (mark)))
      (rectangle-mark-mode 1)
      (goto-char mk))))

;;;###autoload(autoload 'nvp-rect-menu "nvp-rect" nil t)
(transient-define-prefix nvp-rect-menu ()
  [["Move"
    ("h" "←" rectangle-backward-char :transient t)
    ("l" "→" rectangle-forward-char :transient t)
    ("j" "↓" rectangle-next-line :transient t)
    ("k" "↑" rectangle-previous-line :transient t)]
   ["Copy"
    ("d" "Delete" delete-rectangle :transient t)
    ("y" "Yank" yank-rectangle :transient t)
    ("w" "Copy" copy-rectangle-as-kill :transient t)
    ("K" "Kill" kill-rectangle :transient t)
    ("u" "Undo" undo :transient t)]
   ["Modify lines"
    ("s" "Insert string" string-rectangle :transient t)
    ("n" "Number lines" rectangle-number-lines :transient t)
    ("o" "Open region" open-rectangle :transient t)
    ("c" "Clear" clear-rectangle :transient t)]
   ["Mark"
    ("e" "Exchange point" nvp-rect-ex-point-mark :transient t)
    ("r" "Reset mark" nvp-rect-menu--reset :transient t)]]
  (interactive)
  (rectangle-mark-mode 1)
  (transient-setup 'nvp-rect-menu))

(provide 'nvp-rect)
;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not docstrings)
;; End:
;;; nvp-rect.el ends here
