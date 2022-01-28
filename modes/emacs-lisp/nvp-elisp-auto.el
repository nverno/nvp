;;; nvp-elisp-auto.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-project)
  (require 'nvp-test))
(require 'nvp)
(nvp:decls)
(nvp:decl nvp-test-dir nvp-test-find-or-read-matching-test)

;;;###autoload
(defun nvp-elisp-jump-to-cask (&optional this-window)
  "Jump to the closest Cask file."
  (interactive "P")
  (unless (buffer-file-name)
    (user-error "The buffer has no file"))
  (let ((dir (locate-dominating-file (buffer-file-name) "Cask")))
    (unless dir
      (user-error "No Cask file found for this file"))
    (if this-window (find-file (expand-file-name "Cask" dir))
      (find-file-other-window (expand-file-name "Cask" dir)))))

;;; Tests

(defun nvp-elisp--run-tests (&optional prefix regexp)
  (eval-buffer)
  (ert-run-tests-interactively
   (or (and prefix (format "%s--test" prefix))
       regexp
       ".*")))

;; find and run associated ert tests
;;;###autoload
(defun nvp-elisp-run-tests (arg)
  (interactive "P")
  (nvp-with-project (:test-re ".*tests?\.el")
    (nvp-with-test 'local 'create nil nil nil
      (nvp-elisp--run-tests nil (if arg (read-string "Test regexp: "))))))

(provide 'nvp-elisp-auto)
;;; nvp-elisp-auto.el ends here
