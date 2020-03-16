((nil
  (eval                            . (add-to-list 'load-path default-directory)))
 (emacs-lisp-mode
  (mode                            . bug-reference-prog)
  (eval                            . (nvp-buffer-local-set-key
                                      (kbd "C-c C-c") #'nvp-ert-run-tests))))
