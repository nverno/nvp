;;; nvp-find-subrs.el ---  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(require 'nvp-macro)
(require 'nvp)

;; path to local notes file or nil
(defsubst nvp-find-local-notes (&optional name)
  (let ((opts (or name
                  (bound-and-true-p nvp-local-notes-file)
                  nvp-default-notes-files)))
    (if (and (stringp opts) (file-exists-p opts)) (expand-file-name opts)
      (unless (listp opts) (setq opts (list opts)))
      (let* ((case-fold-search t)
             (dir (nvp-file-locate-first-dominating
                   (or (buffer-file-name) default-directory)
                   opts)))
        (when dir
          (cl-some
           (lambda (f)
             (let ((file (expand-file-name f dir)))
               (and (file-exists-p file) file)))
           opts))))))

(provide 'nvp-find-subrs)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-find-subrs.el ends here
