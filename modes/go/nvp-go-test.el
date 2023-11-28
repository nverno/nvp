;;; nvp-go-test.el --- go test -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; FIXME: unused
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'go-mode nil t)
(require 'nvp)
(nvp:decls :p (go))

;; goto end of main function or insert a main function at end if
;; there isn't one
(defun nvp-go--goto-main ()
  (goto-char (point-min))
  (condition-case nil
      (re-search-forward "func[ \t]+main")
    (error
     (goto-char (point-max))
     (insert "\nfunc main() {\n}")
     (forward-line -1)))
  (beginning-of-line)
  (go-end-of-defun))

;; find type(if method), function name, number of parameters, return type
(defun nvp-go--function-info ()
  (when (not (go--in-anonymous-funcion-p))
    (cl-letf (((symbol-function 'forward-word) #'forward-word-strictly))
     (let (name npars rtype class)
       (save-excursion
         (go-goto-function-name t)
         ;; method?
         (and (looking-back "\\_<\\([[:alnum:]]+\\))[ \t]*"
                            (line-beginning-position))
              (setq class (match-string 1)))
         ;; function name
         (setq name (symbol-name (symbol-at-point)))
         (search-forward "(" (line-end-position) 'move)
         ;; count parameters
         (when (not (looking-at-p "[ \t]*)"))
           (setq npars 1)
           (while (search-forward "," (line-end-position) 'move)
             (cl-incf npars)))
         (search-forward ")" (line-end-position) 'move)
         ;; check for return type
         (skip-chars-backward "^)")
         (when (looking-at "[ \t]*\\([[:alnum:]]+\\)[ \t]*{")
           (setq rtype (match-string-no-properties 1)))
         (list name class npars rtype))))))

;; insert formatted print statement for current function into
;; `main'. Expands as yasnippet
;;;###autoload
(defun nvp-go-insert-test ()
  (interactive)
  (let ((info (nvp-go--function-info)))
    (when info
      (cl-destructuring-bind (name class npars rtype) info
        (let* ((stmt
                (format "fmt.Printf(\"%s\\n\", %s(%s))"
                        ;; print return type
                        (cond
                         ((string-prefix-p "int" rtype) "%d")
                         ((string-prefix-p "float" rtype) "%g")
                         ((string-prefix-p "bool" rtype) "%t")
                         ((string-prefix-p "string" rtype) "%s")
                         (t "%v"))
                        (if class
                            (concat "${1:" class "}." name)
                          name)
                        ;; make parameter template
                        (if npars
                            (mapconcat
                             #'(lambda (x)
                                 (concat "$" (int-to-string x)))
                             (number-sequence
                              (if class 2 1) npars) ", ")
                          ""))))
          (push-mark)
          (nvp-go--goto-main)
          (beginning-of-line)
          (insert "\n")
          (forward-line -1)
          (indent-according-to-mode)
          (yas-expand-snippet stmt))))))

(defun nvp-go-test (arg)
  (interactive "P")
  (if (not arg)
      (compile "go test -v")
    ;; (nvp-go-compile-with-completion '("test" "--help") "go test" "go test args: ")
    ))

(defun nvp-go-run-tests (args)
  (interactive)
  (save-selected-window
    (async-shell-command (concat "go test " args))))

(defun nvp-go-run-package-tests ()
  (interactive)
  (nvp-go-run-tests ""))

(defun nvp-go-run-package-tests-nested ()
  (interactive)
  (nvp-go-run-tests "./..."))

(defvar nvp-go-fn-re-1
  "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")

(defvar nvp-go-fn-re-2
  "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?\\([[:alnum:]]+\\))[ ]+\\)?Test[[:alnum:]_]+(.*)")

(defvar go-use-gocheck-for-testing)

(defun nvp-go-run-test-current-function ()
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (let ((test-method
             (if go-use-gocheck-for-testing
                 "-check.f"
               "-run")))
        (save-excursion
          (re-search-backward nvp-go-fn-re-1)
          (nvp-go-run-tests
           (concat test-method "='" (match-string-no-properties 2)
                   "'"))))
    (message
     "Must be in a _test.go file to run go-run-test-current-function.")))

(defun nvp-go-run-test-current-suite ()
  (interactive)
  (if (string-match "_test\.go" buffer-file-name)
      (if go-use-gocheck-for-testing
          (save-excursion
            (re-search-backward nvp-go-fn-re-2)
            (nvp-go-run-tests
             (concat "-check.f='" (match-string-no-properties 2)
                     "'")))
        (message "Gocheck is needed to test the current suite"))
    (message
     "Must be in _test.go file to run go-test-current-suite.")))

(provide 'nvp-go-test)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-go-test.el ends here
