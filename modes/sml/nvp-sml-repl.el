;;; nvp-sml-repl.el --- SML Repl -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-repl)
(nvp:decls)

(defun nvp-sml-inf-newline ()
  (interactive)
  (end-of-line)
  (insert ";")
  (comint-send-input))

;; FIXME(09/22/24): replace with REPL interface
(defvar nvp-sml--last-buffer nil)
(defun nvp-sml-switch-buffers ()
  (interactive)
  (if (and (eq major-mode 'inferior-sml-mode)
           nvp-sml--last-buffer)
      (switch-to-buffer-other-window nvp-sml--last-buffer)
    (setq nvp-sml--last-buffer (current-buffer))
    (sml-prog-proc-switch-to)))

(provide 'nvp-sml-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-sml-repl.el ends here
