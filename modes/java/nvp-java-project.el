;;; nvp-java-project.el --- java project -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'hydra))
(require 'eclim)
(require 'nvp-java)

(autoload 'eclimd--ensure-started "eclimd")
(autoload 'eclim--connected-p "eclim-common")

;; make sure that the eclimd process is running before using hydra
(defun nvp-java-project-pre ()
  (when (not (eclim--connected-p))
    (eclimd--ensure-started t 'nvp-java-project-hydra/body)))

;;;###autoload(autoload 'nvp-java-project-hydra/body "nvp-java-project")
(nvp-hydra-set-property 'nvp-java-project-hydra :verbosity 1)
(defhydra nvp-java-project-hydra (:color blue :pre nvp-java-project-pre)
  "Java Project"
  ("s" nvp-java-new-package  "pkg"       )
  ("b" eclim-project-build   "build"     )  
  ("c" eclim-project-create  "create"    )  
  ("C" eclim-project-close   "close"     )  
  ("D" eclim-project-delete  "del"       )  
  ("g" eclim-project-goto    "goto"      )  
  ("i" eclim-project-import  "import"    )  
  ("n" eclim-project-name    "name"      )  
  ("o" eclim-project-open    "open"      )  
  ("p" eclim-project-mode    "proj-mode" )  
  ("r" eclim-project-refresh "refresh"   )  
  ("R" eclim-project-rename  "rename"    )  
  ("u" eclim-project-update  "update"    )) 

(provide 'nvp-java-project)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-java-project.el ends here
