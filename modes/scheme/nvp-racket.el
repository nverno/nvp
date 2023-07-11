;;; nvp-racket.el --- racket -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls :f (racket-tidy-requires
               racket--buffer-file-name
               racket--do-describe
               racket-back-end-name))
(require 'racket-mode nil t)
(nvp:auto "nvp-hap" nvp-hap-thing-at-point)

(defun nvp-racket-format-buffer ()
  (interactive)
  (racket-tidy-requires))

(defun nvp-hap-racket (command &optional arg &rest _args)
  (cl-case command
    (thingatpt (nvp-hap-thing-at-point (car arg)))
    (doc-buffer
     (let ((how (pcase (get-text-property (point) 'racket-xp-doc)
                  (`(,path ,anchor) `(,path . ,anchor))
                  (_                (racket--buffer-file-name)))))
       (save-window-excursion
         (let ((display-buffer-overriding-action
                '(nil . ((inhibit-switch-frame . t)))))
           (racket--do-describe how nil arg)
           (let ((buf))
             (while (or (not (setq buf (get-buffer (format "*Racket Describe <%s>*"
                                                           (racket-back-end-name)))))
                        (with-current-buffer buf (zerop (buffer-size))))
               (sit-for 0.1))
             (with-current-buffer buf
               (goto-char (point-min))
               (re-search-forward arg nil t 1)
               (goto-char (line-beginning-position -1))
               (list (current-buffer) (point) nil)))))))))

(provide 'nvp-racket)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-racket.el ends here
