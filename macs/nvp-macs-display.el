;;; nvp-macs-display.el --- display macros -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(require 'nvp-macs-common)

(cl-defmacro nvp-with-display-actions
    (prefix
     &rest body
     &key (default '#'ignore) on-frame on-other on-same action-order
     &allow-other-keys)
  "Interpret PREFIX according to `nvp-display-window-get-arguments'.
Bind `display-buffer-overriding-action' and set `current-prefix-arg' for
the next command according to PREFIX. The symbols \\='other-frame,
\\='other-window, and \\='same-window are bound to the display action. If
BODY is non-nil, do BODY. Otherwise, conditionally call interactively do
ON-SAME, ON-OTHER, ON-FRAME or DEFAULT."
  (declare (indent 1))
  (nvp:skip-keywords body)
  (nvp:with-syms (pre)
    `(pcase-let ((`(,,pre ,same-window ,other-window ,other-frame)
                  (nvp-display-window-get-arguments ,prefix ,action-order)))
       (let ((display-buffer-overriding-action
              (cond (other-frame nvp-display-buffer-other-frame-action)
                    (other-window nvp-display-buffer-other-window-action)
                    (same-window nvp-display-buffer-same-window-action))))
         (setq current-prefix-arg ,pre)
         ,(if (null body)
              `(call-interactively
                (cond ,@(and on-other `((other-window ,on-other)))
                      ,@(and on-frame `((other-frame ,on-frame)))
                      ,@(and on-same `((same-window ,on-same)))
                      (t ,default)))
            `(progn ,@body))))))


(provide 'nvp-macs-display)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-macs-display.el ends here
