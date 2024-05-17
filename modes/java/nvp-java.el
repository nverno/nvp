;;; nvp-java.el --- ... -*- lexical-binding: t-*-
;;; Commentary:
;;; TODO:
;; - abbrevs for includes / file-local things only?
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-hap)
  (require 'nvp-compile))
(require 'cc-mode)
(require 'nvp-parse)
(nvp:decls :f (c-syntactic-skip-backward c-syntactic-re-search-forward))

;; -------------------------------------------------------------------
;;; Utils

;; Determine if point is in javadoc comment but not in inline tag,
;; unless OR-INLINE is non-nil
(defun nvp-java-in-javadoc (&optional or-inline)
  (let* ((face (get-text-property (point) 'face))
         (fn (if (symbolp face) #'eq #'memq)))
    (and (funcall fn 'font-lock-doc-face face)
         (or or-inline
             (not (funcall fn 'font-lock-doc-markup-face face))))))

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
  (nvp:defq arg 1)
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
  (nvp:defq arg 1)
  (when (< arg 0)
    (let ((where (c-where-wrt-brace-construct)))
      (when (memq where '(outwith-function))
        (c-syntactic-skip-backward "^}")
        (and (eq (char-before) ?})
             (forward-char -1)))))
  (let ((this-command 'c-end-of-defun))
    (c-end-of-defun arg)))

;;--- Fill
;; FIXME: use `fill-nobreak-predicate'
(defun nvp-java--fill-inline (&optional unfill)
  (let ((from (if unfill "" " "))
        (to (if unfill " " "")))
    (with-silent-modifications
      (-let (((beg . end) (or c-lit-limits
                              (c-literal-limits nil t))))
        (save-excursion
          (goto-char beg)
          (while (re-search-forward "{@[a-z]+[^}\n\r]*}" end t)
            (replace-regexp-in-region
             from to (match-beginning 0) (match-end 0))))))))

(defun nvp-java-fill-paragraph (&optional arg)
  "Like \\[c-fill-paragraph] but doesn't break inline javadoc tags."
  (interactive "*P")
  (if (nvp-java-in-javadoc 'or-inline)
      (progn
        (nvp-java--fill-inline)
        (funcall-interactively #'c-fill-paragraph arg)
        (nvp-java--fill-inline 'unfill))
    (funcall-interactively #'c-fill-paragraph arg)))

;;--- Compile
;; compile SRC and run DRIVER with java -cp CLASSPATH
;; default: javac buffer && java -cp /path/to/buffer Buffer 
(defun nvp-java--compile-and-run-cmd (&optional src classpath driver)
  (nvp:defq
    src (nvp:path 'bf)
    classpath (nvp:path 'dn)
    driver (nvp:path 'bfse))
  (format "javac %s && java -cp %s %s" src classpath driver))

(defun nvp-java-compile-and-run (&optional arg)
  "Compile and run with output to compilation buffer."
  (interactive "P")
  (cond
   ;; can check if main exists: (nvp-tag-list-decls "java" "m")
   ;; ...
   (t (nvp-with-compile-command (nvp-java--compile-and-run-cmd) arg
        (funcall-interactively #'nvp-compile arg 'default)))))

(defun nvp-java-compile (&optional arg)
  (interactive "P")
  (cond
   ;; ((nvp-maven-p) (nvp-maven-compile))
   ;; ((bound-and-true-p eclim-mode) (eclim-project-build))
   ;; ...
   (t (nvp-with-compile-command (format "javac %s" (nvp:path 'bf)) arg
        (funcall-interactively #'nvp-compile arg 'default)))))

;;; Setup
(defun nvp-java-locals ()
  (nvp:setq-local
    beginning-of-defun-function #'nvp-java-beginning-of-defun
    end-of-defun-function #'nvp-java-end-of-defun
    nvp-fill-paragraph-function #'nvp-java-fill-paragraph))

(provide 'nvp-java)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-java.el ends here
