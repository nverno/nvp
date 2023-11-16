;; -*- lexical-binding: t; -*-
(eval-when-compile (defvar yas-text))
(declare-function yas-define-snippets "yasnippet")
(autoload 'conda-env-python-major-version "conda-env")

;; current python version
(defun python-yas-py3-p ()
  (string= "3" (conda-env-python-major-version "python")))

;; Split a python argument string into ((name, default)..) tuples
;; https://github.com/AndreaCrotti/yasnippet-snippets/snippets/python-mode/.yas-setup.el
(defun python-yas-split-args (arg-string)
  (mapcar (lambda (x)
            (split-string x "[[:blank:]]*=[[:blank:]]*" t))
          (split-string arg-string "[[:blank:]]*,[[:blank:]]*" t)))

;; Return docstring format for the python arguments in ARG-STRING
;; https://github.com/AndreaCrotti/yasnippet-snippets/snippets/python-mode/.yas-setup.el
(defun python-yas-args-to-docstring (&optional arg-string)
  (when (and arg-string (> 0 (length arg-string)))
    (let* ((indent (concat "\n" (make-string (current-column) 32)))
           (args (python-yas-split-args arg-string))
           (max-len (if args (apply 'max (mapcar (lambda (x) (length (nth 0 x))) args)) 0))
           (formatted-args
            (mapconcat
             (lambda (x)
               (concat (nth 0 x) (make-string (- max-len (length (nth 0 x))) ? )
                       " -- "
                       (if (nth 1 x) (concat "\(default " (nth 1 x) "\)"))))
             args
             indent)))
      (unless (string= formatted-args "")
        (mapconcat 'identity (list "Keyword Arguments:" formatted-args) indent)))))
