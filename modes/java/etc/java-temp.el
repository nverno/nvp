(eval-when-compile
  (require 'eclim-macros)
  (require 'cl-lib))
(require 'eclim-java)
;; - how to configure eclipse to know where the JRE docs are?
;; - if eclipse knows where the JRE docs are, how to get that info in emacs

;; FIXME: allow creating classes in current directory without creating
;; subdirectories
(defun eclim-java-new (project type name-with-package &optional current-dir)
  "Create a new class of TYPE.  
If CURRENT-DIR is non-nil, create new class in the current directory."
  (interactive (list (eclim-project-name)
                     (eclim--completing-read "Type: " eclim--java-new-types)
                     (read-string "Name: " (eclim--java-current-package))
                     (y-or-n-p "Create in current directory? ")))
  (let ((root-dir (eclim--completing-read "Root: " (eclim/java-src-dirs project))))
    (when eclim-print-debug-messages
      (message "eclim-java-new: project: %s, type: %s, file: %s" project type
               name-with-package))
    (eclim/with-results new-file ("java_new"
                                  ("-p" (if current-dir "" project))
                                  ("-t" type)
                                  ("-n" name-with-package)
                                  ("-r" root-dir))
      (eclim--find-file new-file))))
