;;; nvp-elisp-auto.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

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

(provide 'nvp-elisp-auto)
;;; nvp-elisp-auto.el ends here
