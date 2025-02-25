;;; nvp-rust-cargo.el --- cargo -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'transient)
(require 'rustic nil t)

(nvp:decls :p (rustic) :v (rustic-cargo-test-disable-warnings) :f (cratesio-menu))

(with-eval-after-load 'rustic
  (nvp:transient-toggle nvp-cargo-menu
    rustic-cargo-test-disable-warnings
    rustic-cargo-use-last-stored-arguments)

;;;###autoload(autoload 'nvp-cargo-menu "nvp-rust-cargo" nil t)
  (transient-define-prefix nvp-cargo-menu ()
    [["Run"
      ("r" "Run" rustic-cargo-run)
      ("b" "Build" rustic-cargo-build)
      ("c" "Compile" rustic-compile)
      ("t" "Test" rustic-cargo-test)
      ("B" "Bench" rustic-cargo-bench)
      ("!" "Shell command" rustic-run-shell-command)]
     ["Lint/Format"
      ("C" "Check" rustic-cargo-check)
      ("f" "Format" rustic-cargo-fmt)
      ("l" "Clippy" rustic-cargo-clippy)
      ("F" "Clippy fix" rustic-cargo-clippy-fix)]
     ["Manage Crates"
      ("a" "Add" rustic-cargo-add)
      ("R" "Remove" rustic-cargo-rm)
      ("m" "Add missing" rustic-cargo-add-missing-dependencies)
      ("o" "Outdated" rustic-cargo-outdated)
      ("u" "Update" rustic-cargo-update)
      ("U" "Upgrade" rustic-cargo-upgrade)]
     ["Documentation"
      ("s" "Cratesio" cratesio-menu)
      ("S" "Cargo search" cargo-search-crates)
      ("d" "Open docs" rustic-cargo-doc)
      ("D" "Build docs" rustic-cargo-build-doc)]
     ["Project"
      ("i" "Install" rustic-cargo-install)
      ("K" "Clean" rustic-cargo-clean)
      ("L" "Login" rustic-cargo-login)
      "--"
      ("I" "Init" rustic-cargo-init)
      ("N" "New" rustic-cargo-new)]
     ["Settings"
      (":s" "Use stored args"
       nvp-cargo-menu--toggle-rustic-cargo-use-last-stored-arguments)
      (":w" "Test warnings"
       nvp-cargo-menu--toggle-rustic-cargo-test-disable-warnings)]]))

(provide 'nvp-rust-cargo)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-rust-cargo.el ends here
