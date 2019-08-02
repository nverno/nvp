;;; nvp-image.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro))
(require 'image-mode)

;;;###autoload
(defun nvp-image-fit ()
  (interactive)
  (if (not (eq last-command this-command))
      (image-transform-fit-to-width)
    (setq this-command 'image-transform-fit-to-height)
    (call-interactively 'image-transform-fit-to-height)))

(defmacro image-view (direction)
  `(lambda ()
     (interactive)
     (quit-window)
     (let ((pt (point))
           filename)
       (or (ignore-errors
             (catch 'filename
               (while (dired-next-line ,direction)
                 (when (image-type-from-file-name
                        (setq filename (dired-get-filename)))
                   (throw 'filename filename)))))
           (goto-char pt))
       (dired-view-file))))

(provide 'nvp-image)
;;; nvp-image.el ends here
