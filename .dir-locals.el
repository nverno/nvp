;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (projectile-project-type . emacs-cask))
 ("test" 
  (emacs-lisp-mode
   . ((eval . (local-set-key (kbd "C-c C-c") 'nvp--run-tests))))))
