;;; nvp-rails.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(defvar projectile-tags-file-name)
(nvp:decl projectile-rails-root)

(defvar nvp-rails-buffer "*rails*")
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

;;;###autoload
(defun nvp-rails-update-ctags ()
  "Update ctags for rails project."
  (interactive)
  (let ((default-directory
         (or (projectile-rails-root) default-directory)))
    (shell-command
     (concat "ctags -a -e -f "
             projectile-tags-file-name
             " --tag-relative -R app lib vender test"))))

(provide 'nvp-rails)
;;; nvp-rails.el ends here
