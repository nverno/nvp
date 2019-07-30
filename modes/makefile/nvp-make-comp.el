;;; nvp-make-comp.el --- makefile completion -*- lexical-binding: t; -*-

;;; Commentary:

;; Completion to take into account recursively included files.
;; Parse result from 'make -pqrRns'
;; -p                -r                 -R                     -n           -s
;; --print-data-base --no-builtin-rules --no-builtin-variables --just-print --silent
;; -q
;; --question

;; Variables
;; - after '# Variables'
;; - types: environment, makefile, default, automatic, override
;; - look for preceding 'environment', 'makefile' (from '<file>', line <lineno>)

;; Xref
;; - makefile variables and rules have filenames + locations

;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)

;; (defvar nvp-make-db (make-hash-table :test 'equal))
;; (cl-defstruct nvp-make-dbfile
;;   (:constructor nvp-make--make-dbfile) (:copier nil))

(provide 'nvp-make-comp)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-make-comp.el ends here
