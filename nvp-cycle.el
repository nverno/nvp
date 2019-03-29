;;; nvp-cycle.el --- smart cycling -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-29 00:16:56>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 25 March 2019

;;; Commentary:

;; Cycle between options with single keystroke
;;
;; TODO:
;; - struct to hold options?
;; - set temporary overlay map
;; - after abbrev/snippet hook
;; Using overlays to mark regions like wgrep seems like it might work well.

;; Useful functions:
;; - `cycle-spacing' (#<marker at 37959 in simple.el.gz>)
;;    examples in #<marker at 1838 in smartparens-ess.el>
;; - `just-one-space' (same; wrapper)

;; org-cycle: `org-pre-cycle-hook', `org-cycle-hook'

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'nvp)


(provide 'nvp-cycle)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-cycle.el ends here
