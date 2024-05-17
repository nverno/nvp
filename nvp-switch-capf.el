;;; nvp-switch-capf.el --- command switch completion -*- lexical-binding: t; -*-

;;; Commentary:
;; TODO:
;; - readline completions: how to get in shell-mode?
;; - gud completions have complete cmd - #<marker at 32128 in gud.el.gz>
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp:decls)

;; minibuffer history
(defvar nvp-switch-read-history ())
(defvar-local nvp-switch-args '("--help"))
(defvar-local nvp-switch-cmd ())
(defvar-local nvp-switch--command ())
(defvar-local nvp-switch--candidates ())

;; refreshed when changes to `nvp-switch-cmd' or `nvp-switch-args'
(defun nvp-switch--candidates ()
  (unless nvp-switch-cmd (user-error "nvp-switch-cmd not set locally"))
  (let ((cmd (mapconcat 'identity (cons nvp-switch-cmd nvp-switch-args) " ")))
    (or (and (or (null nvp-switch--command)
                 (equal nvp-switch--command cmd))
             nvp-switch--candidates)
        (setq nvp-switch--command cmd
              nvp-switch--candidates
              (car
               (read-from-string
                (shell-command-to-string
                 (concat
                  nvp-switch--command
                  " | awk 'BEGIN{print \"(\"} /\s+-/ {print \"\\\"\" $1 \"\\\"\"} "
                  "END{print \")\"}'"))))))))

(defun nvp-switch-completion-at-point ()
  (-when-let ((beg . end) (bounds-of-thing-at-point 'symbol))
    (list beg end
          (completion-table-with-cache
           (lambda (_s)
             (nvp-switch--candidates))))))

;;;###autoload
(defun nvp-read-switch (&optional prompt initial-contents cmd set-p &rest args)
  "Read from minibuffer with completion for command line switches from
shell CMD ARGS.
If SET-P is non-nil, set buffer-local values for `nvp-switch-cmd' and
`nvp-switch-args' in the calling buffer."
  (or prompt (setq prompt "Command switches: "))
  ;; make sure local variables from calling buffer are being used
  ;; when completing in the minibuffer
  (with-current-buffer (let ((win (minibuffer-selected-window)))
                         (if (window-live-p win) (window-buffer win)
                           (current-buffer)))
    (let (;(minibuffer-completing-symbol nil)
          (switch-cmd (or cmd nvp-switch-cmd (read-string "Command: ")))
          (switch-args (or args nvp-switch-args (read-string "Args: " "--help"))))
      (when set-p
        (setq nvp-switch-cmd switch-cmd
              nvp-switch-args switch-args))
      (minibuffer-with-setup-hook
          (lambda ()
            (setq nvp-switch-cmd switch-cmd
                  nvp-switch-args switch-args)
            (add-hook 'completion-at-point-functions
                      'nvp-switch-completion-at-point nil 'local))
        (read-from-minibuffer prompt initial-contents
                              read-expression-map nil
                              'nvp-switch-read-history)))))

(provide 'nvp-switch-capf)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-switch-capf.el ends here
