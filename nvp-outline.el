;;; nvp-outline.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'outline)
(require 'transient)

;;;###autoload
(defun nvp-outline-add-locals (&optional arg)
  "Add outline local variables for elisp headers.
Append to `outline-regexp' with prefix."
  (interactive "P")
  (save-excursion
    (goto-char (point-max))
    (insert
     (concat
      "\n;;; Local\svariables:\n"
      ";;; outline-minor-mode: t\n"
      (format ";;; outline-regexp: \"%s\"\n"
              (regexp-quote
               (concat
                (if arg outline-regexp)
                ";;\\*\\|;;;\\*\\|(\\(?:cl-\\)?def[hcuvm]\\|(setq\\|nvp[:]bind")))
      ";;; End:"))))

(defun nvp-outline--exit-hook ()
  (outline-minor-mode -1)
  (remove-hook 'transient-exit-hook #'nvp-outline--exit-hook))

;;;###autoload(autoload 'nvp-outline-menu "nvp-outline" nil t)
(transient-define-prefix nvp-outline-menu ()
  [["Toggle"
    ("SPC" "Children" outline-toggle-children :transient t)
    ("<tab>" "Cycle" outline-cycle :transient t)]
   ["Hide"
    ("B" "All/Body" outline-hide-body :transient t)
    ("S" "Subtree" outline-hide-subtree :transient t)
    ("o" "Other" outline-hide-other :transient t)]
   ["Show"
    ("b" "All" outline-show-all :transient t)
    ("s" "Subtree" outline-show-subtree :transient t)
    ("h" "Headings" outline-show-only-headings :transient t)]]
  [["Move"
    ("u" "Up heading" outline-up-heading :transient t)
    ("j" "Next visible" outline-next-visible-heading :transient t)
    ("k" "Prev visible" outline-previous-visible-heading :transient t)
    ("n" "Forward same level" outline-forward-same-level :transient t)
    ("p" "Backward same level" outline-backward-same-level :transient t)]
   ["Change"
    ("J" "Move down" outline-move-subtree-down :transient t)
    ("K" "Move up" outline-move-subtree-up :transient t)
    ("<" "Promote" outline-promote :transient t)
    (">" "Demote" outline-demote :transient t)
    ("U" "Undo" undo :transient t)
    ("A" "Add locals" nvp-outline-add-locals :transient t)]]
  (interactive)
  (outline-minor-mode 1)
  (add-hook 'transient-exit-hook #'nvp-outline--exit-hook)
  (transient-setup 'nvp-outline-menu))

(provide 'nvp-outline)
;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not docstrings)
;; End:
;;; nvp-outline.el ends here
