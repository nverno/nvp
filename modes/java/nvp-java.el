;;; nvp-java.el --- ... -*- lexical-binding: t-*-
;;; Commentary:
;;; TODO:
;; - abbrevs for includes / file-local things only?
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-hap))
(require 'cc-cmds)
(require 'nvp-parse)

(nvp-decls :f (nvp-compile nvp-abbrev-expand-p))
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
(defun nvp-java-beginning-of-defun (&optional arg)
  "Move backward to the beginning of defun. Wrapper around `c-beginning-of-defun'
that doesn't jump over top-level class decls, but moves into their methods."
  (interactive "p")
  (nvp-defq arg 1)
  (let (where)
    (if (< arg 0)
        (progn
          (c-forward-comments)
          (setq where (c-where-wrt-brace-construct))
          (and (memq where '(at-header))
               (c-syntactic-re-search-forward "{" nil 'eob)))
      (c-backward-comments)
      (setq where (c-where-wrt-brace-construct))
      (when (memq where '(outwith-function at-function-end))
        (c-syntactic-skip-backward "^}")
        (and (eq (char-before) ?})
             (forward-char -1)))))
  (let ((this-command 'c-beginning-of-defun))
    (c-beginning-of-defun arg)))

(defun nvp-java-end-of-defun (&optional arg)
  "Move forward to end of function. Wrapper around `c-end-of-defun'
that doesn't skip class body."
  (interactive "p")
  (nvp-defq arg 1)
  (when (< arg 0)
    (let ((where (c-where-wrt-brace-construct)))
      (when (memq where '(outwith-function))
        (c-syntactic-skip-backward "^}")
        (and (eq (char-before) ?})
             (forward-char -1)))))
  (c-end-of-defun arg))

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

;;; Setup
(defun nvp-java-locals ()
  (nvp-setq-local
    beginning-of-defun-function #'nvp-java-beginning-of-defun
    end-of-defun-function #'nvp-java-end-of-defun))

(provide 'nvp-java)
;;; nvp-java.el ends here
