;;; nvp-c-profile.el ---  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls)

;;;###autoload
(defun nvp-c-callgrind ()
  (interactive)
  (let ((compile-command
         "valgrind --tool=callgrind --callgrind-out-file=callgrind.out ./"))
    (setq current-prefix-arg (list 4))  ; compile with `comint-mode'
    (with-current-buffer (call-interactively #'compile)
      (setq-local nvp-run-default-function
                  (lambda ()
                    (interactive)
                    (start-process "kcachegrind" nil "kcachegrind")))
      (pop-to-buffer (current-buffer))
      (nvp:msg "Call \\<global-map>\\[nvp-run] to run kachegrind"))))

(provide 'nvp-c-profile)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-c-profile.el ends here
