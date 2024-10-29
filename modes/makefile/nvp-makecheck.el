;;; nvp-makecheck.el --- lint/debug makefile -*- lexical-binding: t; -*-

;;; Commentary:
;; - https://github.com/mrtazz/checkmake
;; - make --warn-undefined-variables
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:req 'nvp-makefile 'subrs)
(require 'compile)


(define-compilation-mode checkmake-mode "Makecheck"
  "Compilation mode for makefile linting."
  (setq-local compilation-error-regexp-alist '(makefile))
  (setq-local compilation-error-regexp-alist-alist
              '((makefile
                 "\\(\\([^ \n:]+\\):\\([0-9]+\\)\\) \\(\\w+\\)[: ]\\([^\n]+\\)"
                 2 3 nil 1 1
                 (4 font-lock-builtin-face)
                 (5 font-lock-string-face)))))

;;;###autoload
(defun nvp-makefile-check (&optional targets)
  "Dry run makefile TARGETS to report undefined variables in compilation
 buffer."
  (interactive (list (nvp:makefile-read-targets)))
  (nvp:makefile-with-compilation-vars
   (compilation-start
    (concat "make -n --warn-undefined-variables -f "
            (buffer-file-name) " " targets))))

(defvar nvp-checkmake-args
  '("--format=\"{{.FileName}}:{{.LineNumber}} {{.Rule}} {{.Violation}}
\""))

;;;###autoload
(defun nvp-makefile-checkmake (file &optional args)
  "Run checkmake on makefile."
  (interactive (list (if (or (> (prefix-numeric-value current-prefix-arg) 4)
                             (not (nvp:makefile-p)))
                         (read-file-name "Makefile: ")
                       (buffer-file-name))
                     (or (and current-prefix-arg
                              (list (read-from-minibuffer
                                     "Args: " nvp-checkmake-args)))
                         nvp-checkmake-args)))
  (if-let* ((check (executable-find "checkmake")))
      (let ((args (mapconcat 'identity args " ")))
        (compilation-start (concat check " " args " " file) #'checkmake-mode))
    (user-error "Install checkmake: 'nvp build make lint'")))

;;;###autoload
(defun nvp-makefile-debug (&optional targets)
  "Run makefile TARGETS with shell debugging output."
  (interactive (list (nvp:makefile-read-targets)))
  (nvp:makefile-with-compilation-vars
   (compilation-start
    (concat
     "cat <<TARGET | make -f - run-debug
include " (buffer-file-name) "
_SHELL := \\$(SHELL)
SHELL = \\$(warning [\\$@])\\$(_SHELL) -x
.FORCE run-debug: " targets "
TARGET
"))))

;;;###autoload
(defun nvp-makefile-remake (&optional args)
  "Run remake with ARGS.
Default to dry-run trace."
  (interactive (list (if current-prefix-arg
                         (list (read-string "remake debug: " "-X"))
                       '("--trace" "-n"))))
  (and (listp args)
       (setq args (mapconcat 'identity args " ")))
  (nvp:makefile-with-compilation-vars
   (compilation-start
    (concat "remake " args)
    (not (null (string-match-p (rx (seq symbol-start (or "-X" "--debugger")))
                               args))))))

(provide 'nvp-makecheck)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-makecheck.el ends here
