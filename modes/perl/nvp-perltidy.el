;;; nvp-perltidy.el --- perltidy -*- lexical-binding: t; -*-
;;; Commentary:
;; Call perltidy DWIM
;; modified from https://github.com/genehack/perl-elisp/blob/master/perltidy.el
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'cperl-mode)
(nvp-decls)

(defvar nvp-perltidy "perltidy" "perltidy executable.")
;; FIXME: redirect stderr
(defvar nvp-perltidy-args '("-st" "-se")
  "Args to pass to perltidy. Defaults send outputs to stdout/stderr.")

;; FIXME: abstract out buffer with preview changes -- same as org/yaml-src etc.
;;;###autoload
(defun nvp-perltidy-dwim (arg)
  "Tidy active region, current defun, or entire buffer.
With prefix ARG display the tidy results in separate buffer before applying them."
  (interactive "P")
  (let ((src-buff (current-buffer))
        beg end)
    (cond ((and mark-active transient-mark-mode)
           (setq beg (region-beginning)
                 end (region-end)))
          ((save-excursion
             (beginning-of-defun)
             (when (looking-at-p "\\s-*sub\\s-+")
               (setq beg (point)
                     end (progn (end-of-defun) (point))))))
          (t (setq beg (point-min)
                   end (point-max))))
    (if (not arg)
        (nvp-perltidy-region beg end)
      (let ((perltidy-buffer "*perltidy*")
            (src-bnds (cons beg end)))
        (cl-labels ((apply-changes
                     ()
                     (interactive)
                     (cl-assert (buffer-live-p src-buff))
                     (with-current-buffer src-buff
                       (goto-char (car src-bnds))
                       (save-excursion
                         (delete-region (car src-bnds) (cdr src-bnds))
                         (insert-buffer-substring perltidy-buffer))
                       (pop-to-buffer (current-buffer)))
                     (kill-buffer perltidy-buffer))
                    (abort-changes () (interactive) (kill-buffer perltidy-buffer)))
          (with-temp-buffer-window
           "*perltidy*" t nil
           (with-current-buffer standard-output
             (insert-buffer-substring src-buff beg end)
             (perl-mode)
             (save-excursion (nvp-perltidy-buffer))
             (pop-to-buffer (current-buffer))
             (nvp-set-local-keymap :use t
               ("C-x C-s" . apply-changes)
               ("C-c C-k" . abort-changes))
             (nvp-msg "C-x C-s to apply changes, C-c C-k to abort"))))))))

(defun nvp-perltidy-region (beg end)
  "Call perltidy on region."
  (interactive "r")
  (save-excursion
    (eval
     `(call-process-region ,beg ,end nvp-perltidy t t nil ,@nvp-perltidy-args))))

(defun nvp-perltidy-buffer ()
  "Call perltidy on entire buffer."
  (interactive)
  (nvp-perltidy-region (point-min) (point-max)))

(defun nvp-perltidy-function ()
  "Call perltidy on function at point."
  (interactive)
  (nvp-perltidy-region (progn (beginning-of-defun) (point))
                       (progn (end-of-defun) (point))))

(provide 'nvp-perltidy)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-perltidy.el ends here
