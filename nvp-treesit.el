;;; nvp-treesit.el --- tree-sitter -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'treesit nil t)
(nvp:decls :f (treesit-auto--build-treesit-source-alist))

;;;###autoload
(defun nvp-treesit-install ()
  "Install tree-sitter grammar."
  (interactive)
  (when (require 'treesit-auto nil t)
    (unless treesit-language-source-alist
      (setq treesit-language-source-alist (treesit-auto--build-treesit-source-alist))))
  ;; Notes:
  ;; To remap mode to tree-sitter mode
  ;; (cl-pushnew '(<major-mode> . <treesit-mode>) major-mode-remap-alist :test #'equal)
  ;; May need to restart emacs for shared grammar library to be loaded?
  (call-interactively #'treesit-install-language-grammar))

;;;###autoload
(define-minor-mode nvp-treesit-minor-mode
  "Treesit development minor mode."
  :lighter " T/S"
  :keymap nvp-treesit-mode-map
  ;; XXX: powerline doesn't display `mode-line-misc-info' where
  ;; `treesit-inspect-mode' wants to put the node info
  ;; (powerline-revert)
  (if nvp-treesit-minor-mode
      (progn
        (powerline-revert)
        (treesit-inspect-mode)
        (treesit-explore-mode))
    (treesit-inspect-mode -1)
    (treesit-explore-mode -1)
    (nvp-theme-switch)))

(provide 'nvp-treesit)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-treesit.el ends here
