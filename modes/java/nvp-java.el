;;; nvp-java.el --- ... -*- lexical-binding: t-*-
;;; Commentary:
;;; TODO:
;; - abbrevs for includes / file-local things only?
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-hap))
(require 'nvp-parse)
(nvp-decl nvp-compile nvp-abbrev-expand-p)

(declare-function javadoc-lookup "javadoc-lookup")

;; FIXME: remove
(defun nvp-java-eclipse-releases ()
  (interactive)
  (browse-url "https://projects.eclipse.org/releases"))

;; -------------------------------------------------------------------
;;; Utils

;; FIXME: remove or fix -- these can probably be replaced with eclim
(defmacro nvp-java-method-args ()
  `(save-excursion
     (beginning-of-defun)
     (when (re-search-forward "(\\([^)]*\\))")
       (match-string-no-properties 1))))

;; FIXME: doesn't work 
(defmacro nvp-java-method-name-and-args ()
  `(save-excursion
     (beginning-of-defun)
     (when (re-search-forward "\\([A-Za-z]+\\)\\s-*(")
       (let ((method (match-string-no-properties 1))
             args)
         (while (re-search-forward "\\([A-Za-z]+\\)[\[\]\\s-]*[,)]"
                                   (line-end-position) t)
           (push (match-string-no-properties 1) args))
         (cons method args)))))

;; -------------------------------------------------------------------
;;; Generics
(require 'nvp-parse)

(cl-defmethod nvp-parse-current-function
  (&context (major-mode java-mode) &rest _args)
  (save-excursion
    (beginning-of-defun)
    (search-forward "(")
    (backward-char 2)
    (thing-at-point 'symbol t)))

;; newline
(cl-defmethod nvp-newline-dwim-comment
  (syntax arg &context (major-mode java-mode))
  (nvp-newline-dwim--comment syntax arg " * "))

;; -------------------------------------------------------------------
;;; Commands

;;--- Movement
;; FIXME: convert these to beginning/end-of-defun
;; if outside of class move to methods within instead of just jumping
;; over the whole class
(defun nvp-java-next-defun (&optional arg)
  "Move to next class or method. With ARG, move backwards."
  (interactive)
  (let ((ppss (syntax-ppss)))
    ;; if at start of class, move inside to jump to methods
    (and (= 0 (syntax-ppss-depth ppss))
         (or arg (looking-at-p "\\s-*\\(public\\|private\\|class\\)"))
         (down-list (and arg -1))))
  (beginning-of-defun (and (not arg) -1))
  (recenter))

(defun nvp-java-previous-defun ()
  (interactive)
  (nvp-java-next-defun 'previous))

;;--- Compile

(defun nvp-java-compile ()
  (interactive)
  (cond
   ;; ((nvp-maven-p) (nvp-maven-compile))
   ;; ((bound-and-true-p eclim-mode) (eclim-project-build))
   (t (let ((compile-command
             (format "javac %s && java %s" buffer-file-name
                     (file-name-sans-extension buffer-file-name))))
        (nvp-compile)))))

;;--- Package
;; create package structure in current directory
(defun nvp-java-new-package (root name)
  (interactive (list
                (read-directory-name "Root directory: " default-directory)
                (read-string "Package name: ")))
  (ignore-errors
    (make-directory
     (expand-file-name 
      (concat "src/java/" (replace-regexp-in-string "[.]" "/" name)) root)
     'parents)))

(provide 'nvp-java)
;;; nvp-java.el ends here
