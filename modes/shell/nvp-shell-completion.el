;;; nvp-shell-completion.el --- shell function completion  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Completion for commands in shells
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'pcomplete)
(nvp-decls)

;; allow other completion backends, eg. bash-completion, to take over
;; if pcomplete doesn't find a match
(advice-add 'pcomplete-completions-at-point :filter-return
            (nvp-def nvp@pcomplete-not-exclusive (table)
              (when table
                (nconc table (list :exclusive 'no)))))

;; cache list of github repos
(nvp-lazy-defvar nvp-shell--gh-repo-list
  (lambda ()
    (--when-let
        (shell-command-to-string
         "gh repo list -L 1000 --json name --jq '.[].name' | sort")
      (split-string it "\n" t))))

;; complete for gh CLI
(defun pcomplete/shell-mode/gh ()
  (pcomplete-here '("repo"))
  (when (pcomplete-match "repo" 'first 1)
    (pcomplete-here '("clone")))
  (while (pcomplete-match "^-" 'last) ; skip flags
    (pcomplete-next-arg))
  (while (and (pcomplete-match "clone" 'first 2))
    (pcomplete-here (nvp-lazy-val nvp-shell--gh-repo-list))))

(provide 'nvp-shell-completion)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-shell-completion.el ends here
