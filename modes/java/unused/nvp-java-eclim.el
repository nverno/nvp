(require 'eclim)
(require 'eclimd)

(declare-function eclim--maven-pom-path "eclim-maven")
(declare-function eclim--project-read "eclim-project")
(declare-function eclim--project-dir "eclim-common")
(declare-function eclim--completion-candidates "eclim-completion")
(declare-function eclim-java-browse-documentation-at-point "eclim-javadoc")
(declare-function eclim-javadoc-buffer "eclim-javadoc")

;; -------------------------------------------------------------------
;;; Utils

;; check that it is a maven project
(defsubst nvp-maven-p ()
  (file-exists-p (string-trim (eclim--maven-pom-path))))

;; -------------------------------------------------------------------
;;; Eclim
;; (when (not (bound-and-true-p eclimd-start))
;;   (defalias 'eclimd-start 'start-eclimd))

;; create new project
;;;###autoload
(defun nvp-java-new-project ()
  (interactive)
  (require 'nvp-java)
  (if (not (process-live-p eclimd-process))
      (let ((eclimd-wait-for-process t))
        (eclimd-start eclimd-default-workspace
                      #'(lambda () (call-interactively 'eclim-project-create))))
    (call-interactively 'eclim-project-create)))

;; FIXME: opening shell in project root should be generic
;; open shell in project root
(autoload 'nvp-ext-terminal-in-dir-p "nvp-ext")
(defun nvp-java-project-shell (project-root)
  (interactive
   (let ((root (ignore-errors (eclim--project-dir))))
     (list (file-name-as-directory (or root (eclim--project-read))))))
  (if (not (file-exists-p project-root))
      (message "No project root directory found")
    (let ((default-directory project-root)
          (bname (or (nvp-ext-terminal-in-dir-p 'name project-root)
                     (concat "*java " (eclim-project-name) "*")))
          (curr (current-buffer)))
      (if (get-buffer bname)
          (switch-to-buffer-other-window bname)
        (shell bname))
      (process-put (get-buffer-process bname) :src-buffer curr))))

;; update and refresh current project
(defun nvp-java-update-current-project (project)
  (interactive (list (eclim-project-name)))
  (and project (eclim-project-update project)))

;; info on current project if in one
(defun nvp-java-project-info (&optional project)
  (interactive (list (eclim-project-name)))
  (if project (eclim-project-info-mode project)
    (call-interactively 'eclim-project-info-mode)))

;; -------------------------------------------------------------------
;;; Info

;; returns the variable and type of variable at point or nil, uses eclim's completion
(defun nvp-java-type-name ()
  (save-excursion
    (when-let* ((bnds (bounds-of-thing-at-point 'symbol))
                (var (progn (goto-char (cdr bnds))
                            (car (eclim--completion-candidates)))))
      (when (string-match-p "[:-]" var)
        (let ((res (split-string var "[:-]" nil " ")))
          (pcase (length res)
            (2 res)
            (3 (list (car res) (caddr res) (cadr res))) ;method invocation
            (_ nil)))))))

;; get the fully qualified name if it is imported: eg. java.util.Queue
;; FIXME: find a better way to get this, eg. from the *java doc* buffer
(defun nvp-java-qualified-type-name ()
  (when-let* ((var (nvp-java-type-name)))
    (save-excursion
      (goto-char (point-min))
      (forward-comment (point-max))
      (and (re-search-forward
            (concat "\\s-*import \\s-*\\([^;\n]+"
                    "\\(?:" (car var) "\\|"
                    (replace-regexp-in-string "\\s-*\\([\[\]]+\\|<+[^>]*>+\\)\\s-*"
                                              "" (cadr var))
                    "\\)\\);")
            nil 'move)
           (match-string-no-properties 1)))))

;; TODO: could use much better formatting
;; Show help in a popup window
(defun nvp-java-popup-help ()
  (interactive)
  (when-let* ((doc (save-window-excursion 
                     (call-interactively
                      'eclim-java-show-documentation-for-current-element)
                     (buffer-string))))
    (nvp-with-toggled-tip doc
      :help-fn #'(lambda () (interactive) (pop-to-buffer (eclim-javadoc-buffer))))))

;; simple for now
(defsubst nvp-java-in-javadoc ()
  (nth 4 (syntax-ppss)))

;; Get help on object at point using a variety of methods
;; default is to show help in a popup window -- this assumes that there is
;; local documentation available that eclipse knows about:
;; eg. sudo apt install default-jdk-doc and set eclipse to know about its location
(defun nvp-java-help-at-point (&optional arg)
  (interactive "P")
  (if (nvp-java-in-javadoc)
      (browse-url "https://dev.liferay.com/participate/javadoc-guidelines")
    (pcase arg
      (`(4) (eclim-java-browse-documentation-at-point))
      (`(16) (if-let* ((type (nvp-java-qualified-type-name)))
                 (javadoc-lookup type)
               (call-interactively 'javadoc-lookup)))
      (`(64) (eclim-java-browse-documentation-at-point 'prompt))
      (_ (nvp-java-popup-help)))))

