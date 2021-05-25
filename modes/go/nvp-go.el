;;; nvp-go.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; TODO:
;; - delve debugger setup
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'go-mode)
(nvp-decls :v (go-use-gocheck-for-testing))
(nvp-auto "asdf" asdf-current-version)

(defvar nvp-go-type-re
  (nvp-re-opt '("int" "int8" "int16" "int32" "int64"
                "uint" "uint8" "uint16" "uint32" "uint64"
                "uintptr"
                "float32" "float64"
                "complex64" "complex128"
                "bool" "byte" "rune" "string" "error")))

;; collect imports
(defun nvp-go-imports ()
  (save-excursion
    (goto-char (point-min))
    (let ((import-re "\"\\([^\" \t\n]+\\)")
          res)
      (while (search-forward "import" nil 'move)
        (if (not (looking-at-p "\\s-*("))
            ;; single import
            (when (re-search-forward import-re)
              (push (match-string-no-properties 1) res))
          ;; multiple imports ( ... )
          (skip-chars-forward " \t(")
          (while (not (looking-at-p ")"))
            (when (re-search-forward import-re)
              (push (match-string-no-properties 1) res))
            (forward-line 1))))
      res)))

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

;; -------------------------------------------------------------------
;;; Interactive

;;; Newline dwim
(cl-defmethod nvp-newline-dwim-comment
  (syntax arg &context (major-mode go-mode))
  (nvp-newline-dwim--comment syntax arg " * "))

;;; Insert/Toggle
;; insert import if not there
(defun nvp-go-insert-import (import)
  (unless (member import (nvp-go-imports))
    (save-excursion
      (goto-char (point-min))
      (if (not (search-forward "import" nil 'move))
          (progn
            (search-forward "package" nil 'move)
            (insert (format "\n\nimport (\n\t\"%s\"\n)" import)))
        (if (looking-at-p "\\s-*(")
            (progn
              (skip-chars-forward " \t(")
              (insert (format "\n\"%s\"\n" import)))
          (save-excursion
            (end-of-line)
            (insert "\n)"))
          (insert (format " (\n\t\"%s\"\n" import)))))))

;; insert formatted print statement for current function into
;; `main'. Expands as yasnippet
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

;;; Compile / Run

(defun nvp-go-run-main ()
  (interactive)
  (shell-command
   (format "go run %s" (shell-quote-argument (buffer-file-name)))))

(defun nvp-go-fmt-jump ()
  (interactive)
  (compile "go build -v && go test -v && go vet"))

;; -------------------------------------------------------------------
;;; Tests

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

(provide 'nvp-go)
;;; nvp-go.el ends here
