;;; nvp-rails.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))

(defvar nvp-rails-buffer "*rails-tools*")
(defmacro nvp-rails-buffer ()
  `(with-current-buffer (get-buffer-create nvp-rails-buffer)
     (comint-mode)
     (current-buffer)))

;;;###autoload
(defun nvp-rails-server ()
  (interactive)
  (if (buffer-live-p nvp-rails-buffer)
      (display-buffer nvp-rails-buffer)
    (start-process-shell-command "rails" (nvp-rails-buffer)
                                 "rails server -e development")
    (display-buffer nvp-rails-buffer)
    (browse-url "http://localhost:3000")))

(provide 'nvp-rails)
;;; nvp-rails.el ends here
