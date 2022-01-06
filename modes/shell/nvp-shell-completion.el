;;; nvp-shell-completion.el --- shell function completion  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Completion for commands in shells
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'pcomplete)
(nvp-decls)

;; cache list of github repos
(nvp-lazy-defvar nvp-shell--gh-repo-list
  (lambda ()
    (--when-let
        (shell-command-to-string
         "gh repo list -L 1000 --json name --jq '.[].name' | sort")
      (split-string it "\n" t))))

;; complete for gh CLI
(defun pcomplete/shell-mode/gh ()
  (when (and (pcomplete-arg )))
  (pcomplete-here '("repo"))
  (pcomplete-here '("clone"))
  (while (if (pcomplete-match "^-" 'last) ; skip flags
             (pcomplete-next-arg)
           (pcomplete-here (nvp-lazy-val nvp-shell--gh-repo-list)))))

(provide 'nvp-shell-completion)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-shell-completion.el ends here
