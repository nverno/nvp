;;; nvp-image.el ---  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'image-mode)

;;;###autoload
(defun nvp-image-fit ()
  (interactive)
  (if (not (eq last-command this-command))
      (progn
        (setq image-transform-resize 'fit-width)
        (image-toggle-display-image))
    (setq this-command 'image-transform-fit-to-height)
    (setq image-transform-resize 'fit-height)
    (image-toggle-display-image)))

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
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-image.el ends here
