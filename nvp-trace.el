;;; nvp-trace.el --- trace -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'hydra)
  (require 'nvp-macro))
(require 'trace)

;;;###autoload(autoload 'nvp-trace-hydra/body "nvp-trace")
(nvp-hydra-set-property 'nvp-trace-hydra)
(defhydra nvp-trace-hydra (:color blue )
  ("f" trace-function "trace func")
  ("b" trace-function-background "trace func background")
  ("u" untrace-function "untrace func")
  ("q" untrace-all "untrace all"))

(defvar nvp-trace-group-alist
  '((popups display-buffer
            pop-to-buffer
            pop-to-buffer-same-window
            switch-to-buffer-other-window
            switch-to-buffer-other-frame)))

;;;###autoload
(defun nvp-trace-group (group)
  "Trace GROUP of functions defined in `nvp-trace-group-alist'."
  (interactive (list (completing-read "Trace group: " nvp-trace-group-alist)))
  (dolist (fn (cdr (assoc (intern group) nvp-trace-group-alist)))
    (trace-function-background fn)))

(defun nvp-untrace-group (group)
  "Untrace functions in GROUP."
  (interactive (list (completing-read "Untrace group: " nvp-trace-group-alist)))
  (dolist (fn (cdr (assoc (intern group) nvp-trace-group-alist)))
    (untrace-function fn)))

;; FIXME:
(defun nvp-trace-buffer ()
  (interactive)
  ;; (let (other-window-scroll-buffer)
  ;;   )
  (view-buffer-other-window trace-buffer))

(provide 'nvp-trace)
;;; nvp-trace.el ends here
