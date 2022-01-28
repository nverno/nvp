;;; nvp-maven.el --- maven stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'hydra)
  (require 'nvp-java))
(require 'eclim-maven)

(eval-when-compile
  ;; maven function factory
  (defmacro maven-fn (cmd)
    (unless (stringp cmd) (setq cmd (symbol-name cmd)))
    `(defun ,(intern (concat "nvp-maven-" cmd)) ()
       (interactive)
       (if (nvp-maven-p)
           (eclim-maven-run ,cmd)
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
(nvp:hydra-set-property 'nvp-maven-hydra :verbosity 1)
(defhydra nvp-maven-hydra (:color blue)
  ("r" eclim-maven-run "run")
  ("p" eclim-maven-lifecycle-phase-run "phase")
  ("c" nvp-maven-compile "compile")
  ("t" nvp-maven-test "test")
  ("i" nvp-maven-install "install")
  ("C" nvp-maven-clean-install "clean-install")
  ("d" eclim-debug-maven-test "debug")
  ("q" nil))

(provide 'nvp-maven)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-maven.el ends here
