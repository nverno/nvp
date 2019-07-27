;;; nvp-coffee-help.el --- Lookup help at coffeescript.org from emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;  Interface to jump to sections at http://coffeescript.org to lookup
;;  help documentation from emacs.
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))

(defvar nvp-coffee-help--data-location nil)
(setq nvp-coffee-help--data-location
      (when load-file-name
        (expand-file-name "build/index.el" (file-name-directory load-file-name))))

(defvar nvp-coffee-help--data nil)

(defun nvp-coffee-help--load-alist (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((data (car
                 (read-from-string
                  (buffer-substring-no-properties
                   (point-min) (point-max))))))
      (setq nvp-coffee-help--data data)))
  nvp-coffee-help--data)

;;;###autoload
(defun nvp-coffee-help-online ()
  "Jump to location at http://coffeescript.org."
  (interactive)
  (let* ((data (or nvp-coffee-help--data
                   (nvp-coffee-help--load-alist nvp-coffee-help--data-location)))
         (section (ido-completing-read "Location: " data)))
    (browse-url (concat "http://coffeescript.org/"
                        (cdr (assoc section data))))))

(provide 'nvp-coffee-help)
;;; nvp-coffee-help.el ends here
