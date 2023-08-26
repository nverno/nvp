;;; nvp-treesit.el --- tree-sitter -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'treesit nil t)
(nvp:decls :p ("treesit-auto"))

;; Setup treesitter sources
(defun nvp-treesit--setup-sources ()
  (when (require 'treesit-auto nil t)
    (unless treesit-language-source-alist
      (setq treesit-language-source-alist (treesit-auto--build-treesit-source-alist)))))

;; maybe load sources
(cl-eval-when (load)
  (nvp-treesit--setup-sources))

;;;###autoload
(defun nvp-treesit-install ()
  "Install tree-sitter grammar."
  (interactive)
  (nvp-treesit--setup-sources)
  ;; Notes:
  ;; To remap mode to tree-sitter mode
  ;; (cl-pushnew '(<major-mode> . <treesit-mode>) major-mode-remap-alist :test #'equal)
  ;; May need to restart emacs for shared grammar library to be loaded?
  (call-interactively #'treesit-install-language-grammar))

;; -------------------------------------------------------------------
;;; Dev Minor Mode

(defun nvp-treesit-validate (lang query)
  (interactive
   (list
    (or (treesit-language-at (point))
        (completing-read "Language: " (mapcar #'car treesit-language-source-alist)))
    (read-from-minibuffer "Query: " (sexp-at-point))))
  (treesit-query-validate lang query))

(defvar-keymap nvp-treesit-minor-mode-map
  :doc "T/S keymap"
  "C-c v" #'nvp-treesit-validate
  "C-M-?" #'treesit-inspect-node-at-point)

(easy-menu-define nvp-treesit-minor-mode-menu nvp-treesit-minor-mode-map
  "T/S menu."
  '("T/S"
    ["Inspect" treesit-inspect-node-at-point t]
    ["Validate" nvp-treesit-validate t]))

;;;###autoload
(define-minor-mode nvp-treesit-minor-mode
  "Treesit development minor mode."
  :lighter " T/S"
  :keymap nvp-treesit-minor-mode-map
  ;; XXX: powerline doesn't display `mode-line-misc-info' where
  ;; `treesit-inspect-mode' wants to put the node info
  ;; (powerline-revert)
  (if nvp-treesit-minor-mode
      (when (treesit-language-at (point))
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
