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

(provide 'nvp-trace)
;;; nvp-trace.el ends here
