;;; nvp-racket.el --- racket -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'racket-mode nil t)
(nvp:decls :p ("racket"))
(nvp:auto "nvp-hap" nvp-hap-thing-at-point)

(with-eval-after-load 'nvp-repl
  (require 'nvp-racket-repl))

(defun nvp-racket-expand (&optional choose)
  "Expand dwim without macro hiding.
If region is active, expand the region.
Else if prefix CHOOSE, select thing to expand.
Else if point is at the end of a list expand the previous sexp.
Otherwise expand the list containing point."
  (interactive "P")
  (let ((fn (cond
             ((region-active-p) #'racket-expand-region)
             (choose (nvp:read-char-case "Expand: " 'verbose
                       (?s "[s]sexp" #'racket-expand-last-sexp)
                       (?d "[d]ef" #'racket-expand-definition)
                       (?f "[f]ile" #'racket-expand-file)))
             ((eq ?\) (char-before)) #'racket-expand-last-sexp)
             (t nil))))
    (if fn (funcall fn 'no-hiding)
      (racket-stepper--expand-text
       t (lambda () (bounds-of-thing-at-point 'list))))))

;; -------------------------------------------------------------------
;;; Help

;; XXX: when is this better than using `racket--xp-make-company-doc-buffer-proc'
(defun nvp-hap--racket-describe (thing args)
  (let ((how (if args (racket--buffer-file-name)
               (pcase (get-text-property (point) 'racket-xp-doc)
                 (`(,path ,anchor) `(,path . ,anchor))
                 (_                (racket--buffer-file-name))))))
    (save-window-excursion
      (let ((display-buffer-overriding-action
             ;; FIXME: still flashes buffer
             ;; '(display-buffer-no-window)
             '(nil . ((inhibit-switch-frame . t)))))
        (racket--do-describe how nil thing)
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
                        (when (re-search-forward thing nil t 1)
                          (goto-char (line-beginning-position -1))
                          (point))
                        (point-min)))
                  (end (save-excursion
                         (forward-line 1)
                         (when (re-search-forward "\\s-*---+" nil t 1)
                           (line-beginning-position)))))
              (list (current-buffer) beg end))))))))

;; `nvp-hap-backend' help-at-point function for Racket using `racket-mode' to
;; parse online documentation
(defun nvp-hap-racket (command &optional arg &rest args)
  (cl-case command
    (thingatpt (nvp-hap-thing-at-point arg nil nil #'racket--describe-terms))
    (doc-buffer                         ; args has prefix
     (or (--when-let (funcall (racket--xp-make-company-doc-buffer-proc) arg)
           (list it (point-min)))
         (nvp-hap--racket-describe arg args)))))

(provide 'nvp-racket)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-racket.el ends here
