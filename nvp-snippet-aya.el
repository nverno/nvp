;;; nvp-snippet-aya.el --- auto-snippets -*- lexical-binding: t; -*-

;;; Commentary:
;; auto yasnippets
;;; Code:

(eval-when-compile (require 'nvp-macro))
(require 'transient)
(require 'auto-yasnippet nil t)
(nvp:decls :p (aya) :f (nvp-jump-to-new-snippet) :v (aya-current))

(defconst nvp-aya-new-template "\
# -*- mode: snippet -*-
# name: $1
# key: ${2:${1:$(yas--key-from-desc yas-text)}}
# --
`aya-current`")

;;;###autoload(autoload 'nvp-aya-menu "nvp-snippet-aya" nil t)
(transient-define-prefix nvp-aya-menu ()
  ["Auto-Snippet"
   ("c" "Create(~)" aya-create)
   ("e" "Expand" aya-expand)
   ("j" "Jump/persist" nvp-jump-to-aya-snippet)
   ("o" "One-liner($)" aya-create-one-line)])

;;;###autoload
(defun nvp-jump-to-aya-snippet ()
  "Save previously created auto-yasnippet."
  (interactive
   (if (string-empty-p aya-current)
       (user-error "No current auto-snippet.")
     (list aya-current)))
  (nvp-jump-to-new-snippet major-mode nvp-mode-snippet-dir nil aya-current
                           nvp-aya-new-template))

(provide 'nvp-snippet-aya)
;;; nvp-snippet-aya.el ends here
