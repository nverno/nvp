;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (projectile-project-type . emacs-cask)
  (nvp-local-notes-file . "~/org/code/emacs.org"))
 (emacs-lisp-mode
  (outline-regexp . "\f\\|\\`;\\|;;\\*\\|;;;\\*\\|(def[cvu]\\|(setq\\|;;;;\\*")
  (indent-tabs-mode . nil)
  (fill-column . 80)
  (emacs-lisp-docstring-fill-column . 75))
 ("test"
  (nil . ((eval . (add-to-list 'load-path default-directory))))
  (emacs-lisp-mode
   . ((eval . (nvp-buffer-local-set-key (kbd "C-c C-c") #'nvp-ert-run-tests))
      (no-byte-compile . t))))
 ("old"
  (nil . ((no-byte-compile . t))))
 ("etc"
  (nil . ((no-byte-compile . t))))
 ("script"
  (nil . ((no-byte-compile . t)))))
