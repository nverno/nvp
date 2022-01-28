;;; nvp-outline.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'outline)

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
                ";;\\*\\|;;;\\*\\|(\\(?:cl-\\)?def[hcuvm]\\|(setq\\|nvp:bind")))
      ";;; End:"))))

;;;###autoload(autoload 'nvp-outline-hydra/body "nvp-outline")
(nvp:hydra-set-property 'nvp-outline-hydra)
(defhydra nvp-outline-hydra (:color red)
  "
^Hide^              ^Show^           ^Move
^^^^^^------------------------------------------------------
_h_: body            _l_: all         _u_: up
_o_: other           _SPC_: toggle    _j_: next visible
                                  _k_: previous visible
_<backtab>_: subtree _TAB_: subtree   _n_: forward same level
                                  _p_: backward same level
"
  ;; Hide
  ("h" outline-hide-body)                 ; Hide everything but headings (all body)
  ("o" outline-hide-other)                ; Hide other branches
  ("<backtab>" outline-hide-subtree)      ; Hide everything in entry and sub-entries
  ;; Show
  ("l" outline-show-all)                  ; Show (expand) everything
  ("TAB" outline-show-subtree)            ; Show (expand) everything in heading & below
  ("SPC" outline-toggle-children)
  ;; Move
  ("u" outline-up-heading)                ; Up
  ("j" outline-next-visible-heading)      ; Next
  ("k" outline-previous-visible-heading)  ; Previous
  ("n" outline-forward-same-level)        ; Forward - same level
  ("p" outline-backward-same-level)       ; Backward - same level
  ("q" nil "quit"))

(provide 'nvp-outline)
;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not docstrings)
;; End:
;;; nvp-outline.el ends here
