;;; nvp-racket.el --- racket -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls :f (racket-tidy-requires
               racket--buffer-file-name
               racket--do-describe
               racket--describe-goto
               racket-back-end-name))
(require 'racket-mode nil t)
(nvp:auto "nvp-hap" nvp-hap-thing-at-point)

(defun nvp-racket-expand ()
  (interactive)
  (call-interactively
   (if (region-active-p)
       #'racket-expand-region
     (nvp:read-char-case "Expand: " 'verbose
       (?s "[s]sexp" #'racket-expand-last-sexp)
       (?d "[d]ef" #'racket-expand-definition)
       (?f "[f]ile" #'racket-expand-file)))))

;; `nvp-hap-backend' help-at-point function for Racket using `racket-mode' to
;; parse online documentation
(defun nvp-hap-racket (command &optional arg &rest args)
  (cl-case command
    (thingatpt (nvp-hap-thing-at-point arg nil nil (racket--describe-terms)))
    (doc-buffer                         ; args has prefix
     (let ((how (if args (racket--buffer-file-name)
                    (pcase (get-text-property (point) 'racket-xp-doc)
                      (`(,path ,anchor) `(,path . ,anchor))
                      (_                (racket--buffer-file-name))))))
       (save-window-excursion
         (let ((display-buffer-overriding-action
                '(nil . ((inhibit-switch-frame . t)))))
           (racket--do-describe how nil arg)
           (let (buf)
             (while (or (not (setq buf (get-buffer (format "*Racket Describe <%s>*"
                                                           (racket-back-end-name)))))
                        (with-current-buffer buf (zerop (buffer-size))))
               (sit-for 0.1))
             (with-current-buffer buf
               (let ((beg (or
                           ;; use `racket--describe-goto' to locate symbol doc
                           (and (consp how)
                                (cdr how)
                                (cdr (racket--describe-goto (cdr how))))
                           (when (re-search-forward arg nil t 1)
                             (goto-char (line-beginning-position -1))
                             (point))
                           (point-min)))
                     (end (save-excursion
                            (forward-line 1)
                            (when (re-search-forward "\\s-*---+" nil t 1)
                              (line-beginning-position)))))
                 (list (current-buffer) beg end))))))))))

(provide 'nvp-racket)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-racket.el ends here
