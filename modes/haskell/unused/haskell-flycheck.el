;;; flycheck stack-ghci  -*- lexical-binding: t; -*-
;; http://ioctl.it/posts/2015-07-03-stack-flycheck.html
;; (when (eq haskell-process-type 'stack-ghci))
(require 'flycheck)

;; Reconfigure flycheck haskell settings, e.g after changing cabal file.
(defun haskell-tools-flycheck-recon ()
  (interactive)
  (flycheck-haskell-clear-config-cache)
  (flycheck-haskell-configure)
  (flycheck-mode -1)
  (flycheck-mode))

(flycheck-define-checker
    haskell-stack
  "A Haskell syntax and type checker using ghc. See URL 
`http://www.haskell.org/ghc/'."
  :command ("stack" "ghc" "--" "-Wall" "-fno-code"
            (option-flag "-no-user-package-db"
                         flycheck-ghc-no-user-package-database)
            (option-list "-package-db" flycheck-ghc-package-databases)
            (option-list "-i" flycheck-ghc-search-path concat)
            ;; Include the parent directory of the current module tree, to
            ;; properly resolve local imports
            (eval (concat
                   "-i"
                   (flycheck-module-root-directory
                    (flycheck-find-in-buffer flycheck-haskell-module-re))))
            (option-list "-X" flycheck-ghc-language-extensions concat)
            (eval flycheck-ghc-args)
            "-x" (eval
                  (pcase major-mode
                    (`haskell-mode "hs")
                    (`literate-haskell-mode "lhs")))
            source)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":"
            (or " " "\n    ") "Warning:" (optional "\n")
            (message
             (one-or-more " ") (one-or-more not-newline)
             (zero-or-more "\n"
                           (one-or-more " ")
                           (one-or-more not-newline)))
            line-end)
   (error line-start (file-name) ":" line ":" column ":"
          (or (message (one-or-more not-newline))
              (and "\n"
                   (message
                    (one-or-more " ") (one-or-more not-newline)
                    (zero-or-more "\n"
                                  (one-or-more " ")
                                  (one-or-more not-newline)))))
          line-end))
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors (flycheck-dedent-error-messages errors)))
  :modes (haskell-mode literate-haskell-mode)
  :next-checkers ((warning . haskell-hlint)))

(provide 'haskell-flycheck)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; haskell-flycheck.el ends here
