;; -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'nvp-macro)
  (nvp-local-vars)
  (require 'cl-lib))
(require 'java-tools)
(require 'eclim-maven)
(require 'hydra)

(eval-when-compile
  (defmacro stringify (cmd)
    `(if (stringp ,cmd) ,cmd (symbol-name ,cmd)))

  ;; maven function factory
  (defmacro maven-fn (cmd)
    `(defun ,(intern (concat "nvp-maven-" (stringify cmd))) ()
       (interactive)
       (if (nvp-maven-p)
           (eclim-maven-run ,(stringify cmd))
         (message "No pom.xml file found in project root directory")))))

;; -------------------------------------------------------------------
;;; Commands 

;; run using eclim-maven-run
;;;###autoload(autoload 'nvp-maven-test "nvp-maven")
(maven-fn test)
;;;###autoload(autoload 'nvp-maven-clean-install "nvp-maven")
(maven-fn clean-install)
;;;###autoload(autoload 'nvp-maven-install "nvp-maven")
(maven-fn install)
;;;###autoload(autoload 'nvp-maven-compile "nvp-maven")
(maven-fn compile)

;;;###autoload(autoload 'nvp-maven-hydra/body "nvp-maven")

(defhydra nvp-maven-hydra (:color blue)
  ("r" eclim-maven-run "run")
  ("p" eclim-maven-lifecycle-phase-run "phase")
  ("c" nvp-maven-compile "compile")
  ("t" nvp-maven-test "test")
  ("i" nvp-maven-install "install")
  ("C" nvp-maven-clean-install "clean-install")
  ("d" eclim-debug-maven-test "debug")
  ("q" nil))
(hydra-set-property 'nvp-maven-hydra :verbosity 1)

(provide 'nvp-maven)
