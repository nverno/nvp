;;; nvp-macs-display.el --- output macros -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; - results buffers
;;
;;; Code:
(require 'nvp-macs-common)
(nvp-decls :f (nvp-window-configuration-restore nvp-window-configuration-save))

(defmacro nvp-with-results-buffer (&optional buffer-or-name title &rest body)
  "Do BODY in temp BUFFER-OR-NAME as with `with-temp-buffer-window'.
Make the temp buffer scrollable, in `view-mode' and kill when finished."
  (declare (indent 2) (debug (sexp &rest form)))
  `(let (other-window-scroll-buffer)
     (nvp-window-configuration-save)
     (with-temp-buffer-window
         ,(or buffer-or-name '(help-buffer))
         t
         nil
       (with-current-buffer standard-output
         (setq other-window-scroll-buffer (current-buffer))
         ,(if title `(princ (nvp-centered-header ,title)))
         ,@body
         (hl-line-mode)
         (view-mode-enter nil #'nvp-window-configuration-restore)))))

(provide 'nvp-macs-display)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-macs-display.el ends here
