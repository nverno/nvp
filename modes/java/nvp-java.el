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

;; -------------------------------------------------------------------
;;; Utils

;; for snippets: FIXME: check actually javadoc
(defun nvp-java-in-javadoc ()
  (nth 4 (syntax-ppss)))

;; FIXME: remove or fix -- these can probably be replaced with eclim
(defmacro nvp-java-method-args ()
  `(save-excursion
     (beginning-of-defun)
     (when (re-search-forward "(\\([^)]*\\))")
       (match-string-no-properties 1))))

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
;; compile SRC and run DRIVER with java -cp CLASSPATH
;; default: javac buffer && java -cp /path/to/buffer Buffer 
(defun nvp-java--compile-and-run-cmd (&optional src classpath driver)
  (nvp-defq
    src (nvp-path 'bf)
    classpath (nvp-path 'dn)
    driver (nvp-path 'bfse))
  (format "javac %s && java -cp %s %s" src classpath driver))

(defun nvp-java-compile-and-run (arg)
  "Compile and run with output to compilation buffer."
  (interactive "P")
  (cond
   ;; ...
   (t (let ((compile-command (nvp-java--compile-and-run-cmd)))
        (funcall-interactively #'nvp-compile arg)))))

(defun nvp-java-compile ()
  (interactive)
  (cond
   ;; ((nvp-maven-p) (nvp-maven-compile))
   ;; ((bound-and-true-p eclim-mode) (eclim-project-build))
   ;; ...
   (t (let ((compile-command (format "javac %s" (nvp-path 'bf))))
        (nvp-compile)))))

;;; Setup
(defun nvp-java-locals ()
  (nvp-setq-local
    beginning-of-defun-function #'nvp-java-beginning-of-defun
    end-of-defun-function #'nvp-java-end-of-defun))

(provide 'nvp-java)
;;; nvp-java.el ends here
