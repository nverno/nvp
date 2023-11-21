;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (projectile-project-type            . elisp)
  (projectile-project-compilation-cmd . "make rebuild")
  (projectile-project-test-cmd        . "make test")
  (projectile-project-configure-cmd   . "make check-compiled")
  (projectile-project-install-cmd     . "make unicode")
  (bug-reference-url-format           . "https://github.com/nverno/%s"))
 (emacs-lisp-mode
  ;; (outline-regexp                  . "\f\\|\\`;\\|;;\\*\\|;;;\\*\\|(def[cvu]\\|(setq\\|;;;;\\*")
  (indent-tabs-mode                   . nil)
  (fill-column                        . 80)
  (emacs-lisp-docstring-fill-column   . 75))
 ("test"
  (nil
   (eval                              . (add-to-list 'load-path default-directory)))
  (emacs-lisp-mode
   (mode                              . bug-reference-prog)
   (eval                              . (nvp-buffer-local-set-key
                                         (kbd "C-c C-c") #'nvp-ert-run-tests))
   (no-byte-compile                   . t)))
 ("old"
  (nil (no-byte-compile               . t)))
 ("etc"
  (nil (no-byte-compile               . t)))
 ("unused"
  (nil (no-byte-compile               . t)))
 ("script"
  (nil (no-byte-compile               . t))))
