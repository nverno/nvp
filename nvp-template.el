;;; nvp-template.el --- expand templates -*- lexical-binding: t; -*-

;;; Commentary:
;; see grep.el, time-stamp.el
;; - use `format-time-string' to expand times

;; Some from gud.el - #<marker at 13405 in gud.el.gz>
;; %f -- Name (w/o directory) of current source file
;; %F -- Name (w/o directory & extension) of current source file
;; %d -- Directory of current source file.
;; %l -- Number of current source file.
;; %p -- Prefix argument of command (if any) as a number.

;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)


(provide 'nvp-template)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-template.el ends here
