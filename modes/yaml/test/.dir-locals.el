((nil
  (eval                            . (add-to-list 'load-path default-directory)))
 (emacs-lisp-mode
  (mode                            . bug-reference-prog)
  (eval                            . (nvp-buffer-local-set-key
                                      (kbd "C-c C-c")
                                      (lambda ()
                                        (interactive)
                                        (let ((default-directory
                                               (f-parent
                                                (f-dirname (buffer-file-name)))))
                                          (call-interactively #'nvp-ert-run-tests)))))))
