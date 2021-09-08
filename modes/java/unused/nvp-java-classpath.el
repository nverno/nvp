;; -*- lexical-binding: t; -*-
(eval-when-compile (require 'nvp-macro))
(eval-and-compile (require 'hydra))
(require 'eclim)

;; TODO: unimplemented?
(defun nvp-eclim-classpath-create (lib-dir)
  (interactive "D")
  (eclim--call-process 
   "java_classpath_variable_create" "-n" "lib" "-p" lib-dir))
