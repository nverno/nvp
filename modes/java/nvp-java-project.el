;;; nvp-java-project ---  -*- lexical-binding: t; -*-
(eval-when-compile (require 'nvp-macro))
(eval-and-compile (require 'hydra))
(require 'eclim)
(require 'java-tools)

(autoload 'eclimd--ensure-started "eclimd")
(autoload 'eclim--connected-p "eclim-common")

;; make sure that the eclimd process is running before using hydra
(defun nvp-java-project-pre ()
  (when (not (eclim--connected-p))
    (eclimd--ensure-started t 'nvp-java-project-hydra/body)))

;;;###autoload
(defun nvp-java-project ()
  (interactive)
  (require 'nvp-java)
  (nvp-bind-keys nvp-project-keymap
    ("c j" . nvp-java-project-hydra/body))
  (nvp-java-project-hydra/body))

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
(hydra-set-property 'nvp-java-project-hydra :verbosity 1)

(provide 'nvp-java-project)
;;; nvp-java-project.el ends here
