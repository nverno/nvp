;;; nvp-macs-display.el --- display macros -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(require 'nvp-macs-common)


(cl-defmacro nvp-with-no-window
    (&rest body &key (actions '(no-window)) &allow-other-keys)
  "Do BODY with overriding display ACTIONS to inhibit displaying windows."
  (declare (indent defun) (debug t))
  (nvp:skip-keywords body)
  `(let (display-buffer-alist
         (display-buffer-overriding-action
          (apply #'nvp-display-buffer-action ',actions)))
     ,@body))


(cl-defmacro nvp-with-display-actions
    (prefix
     &rest body
     &key (default '#'ignore) on-frame on-other on-same action-order
     same-action other-action frame-action override-action
     &allow-other-keys)
  "Interpret PREFIX according to `nvp-display-window-get-arguments'.

Bind `display-buffer-overriding-action' and set `current-prefix-arg' for
the next command according to PREFIX. The symbols \\='other-frame,
\\='other-window, and \\='same-window are bound to the display action.

OVERRIDE-ACTION unconditionally sets `display-buffer-overriding-action'. If
it is one of \\='nil (must be quoted!), t or :none, then
`display-buffer-overriding-action' is set to nil.

If BODY is non-nil, do BODY. Otherwise, conditionally call interactively do
ON-SAME, ON-OTHER, ON-FRAME or DEFAULT."
  (declare (debug t) (indent defun))
  (nvp:skip-keywords body)
  (nvp:with-syms (pre prefix-p)
    `(pcase-let ((`(,,pre ,same-window ,other-window ,other-frame)
                  (nvp-display-window-get-arguments ,prefix ,action-order)))
       (setq current-prefix-arg ,pre)
       ,(macroexp-let2 nil override-action override-action
          (let ((no-override-p
                 (member override-action (list t :none ''nil))))
            `(let* (,@(unless no-override-p
                        `((,prefix-p (or nvp-display-prefix-p
                                         (memq last-command
                                               '(same-window-prefix
                                                 other-window-prefix
                                                 other-frame-prefix
                                                 nvp-window-prefix))))))
                    ,@(unless no-override-p
                        `((display-buffer-overriding-action
                           ,(if override-action
                                `(or (and (not ,prefix-p) ,override-action)
                                     display-buffer-overriding-action)
                              `(cond
                                (,prefix-p display-buffer-overriding-action)
                                (other-frame
                                 ,(or frame-action
                                      'nvp-display-buffer-other-frame-action))
                                (other-window
                                 ,(or other-action
                                      'nvp-display-buffer-other-window-action))
                                (same-window
                                 ,(or same-action
                                      'nvp-display-buffer-same-window-action))))))))
               ,(if (null body)
                    `(call-interactively
                      (cond ,@(and on-other `((other-window ,on-other)))
                            ,@(and on-frame `((other-frame ,on-frame)))
                            ,@(and on-same `((same-window ,on-same)))
                            (t ,default)))
                  `(progn ,@body))))))))


(provide 'nvp-macs-display)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-macs-display.el ends here
