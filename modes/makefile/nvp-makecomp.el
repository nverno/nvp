;;; nvp-makecomp.el --- makefile completion -*- lexical-binding: t; -*-

;;; Commentary:

;; Completion and Xref backends for makefile variables from current
;; buffer and included files:
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
(eval-when-compile
  (require 'nvp-macro))
(require 'nvp)

(defvar nvp-makecomp-program
  (expand-file-name "bin/makevars.awk" (nvp-path 'dn/ (nvp-load-file-name))))

(cl-defstruct (nvp-makecomp-db
               (:constructor nvp-makecomp-make-db)
               (:copier nil))
  filename
  variables                             ;variables from included files as well
  includes
  modtime)

(defvar nvp-makecomp-data (nvp-makecomp-make-db))

(defun nvp-makecomp--update (&optional file)
  (when (or file (setq file (buffer-file-name)))
    (-when-let (data (car
                      (read-from-string
                       (shell-command-to-string
                        (format "make -pqrRns %s | %s"
                                (nvp-path 'dn/ file) nvp-makecomp-program)))))
      (setf (nvp-makecomp-db-variables nvp-makecomp-data)
            (alist-get 'variables data))
      (setf (nvp-makecomp-db-filename nvp-makecomp-data) file))))

;; (defvar nvp-make-db (make-hash-table :test 'equal))
;; (cl-defstruct nvp-make-dbfile
;;   (:constructor nvp-make--make-dbfile) (:copier nil))

(provide 'nvp-make-comp)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-makecomp.el ends here
