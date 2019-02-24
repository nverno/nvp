;;; nvp-trace.el --- trace -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-24 04:48:11>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 12 February 2019

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
