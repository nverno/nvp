(eval-when-compile
  (defvar yas-text)
  (defvar start-point))
(declare-function yas-define-snippets "yasnippet")
(autoload 'conda-env-python-major-version "conda-env")

(add-hook 'python-mode-hook
          #'(lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))

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

;; Return information on the current definition.
;; (defun elpy-snippet-current-method-and-args ()
;;   (let ((current-defun (python-info-current-defun))
;;         (current-arglist
;;          (save-excursion
;;            (python-nav-beginning-of-defun)
;;            (when (re-search-forward "(" nil t)
;;              (let* ((start (point))
;;                     (end (progn
;;                            (forward-char -1)
;;                            (forward-sexp)
;;                            (- (point) 1))))
;;                (python-yas-split-args
;;                 (buffer-substring-no-properties start end))))))
;;         class method args)
;;     (when (not current-arglist)
;;       (setq current-arglist '(("self"))))
;;     (if (and current-defun
;;              (string-match "^\\(.*\\)\\.\\(.*\\)$" current-defun))
;;         (setq class (match-string 1 current-defun)
;;               method (match-string 2 current-defun))
;;       (setq class "Class"
;;             method "method"))
;;     (setq args (mapcar #'car current-arglist))
;;     (list class method args)))

;; Return the typical __init__ assignments for arguments.
;; (defun elpy-snippet-init-assignments (arg-string)
;;   (let ((indentation (make-string (save-excursion
;;                                     (goto-char start-point)
;;                                     (current-indentation))
;;                                   ?\s)))
;;     (mapconcat (lambda (arg)
;;                  (if (string-match "^\\*" (car arg))
;;                      ""
;;                    (format "self.%s = %s\n%s"
;;                            (car arg)
;;                            (car arg)
;;                            indentation)))
;;                (python-yas-split-args arg-string)
;;                "")))

;; Return (Class, first-arg).method if Py2.
;; Else return ().method for Py3.
;; (defun elpy-snippet-super-form ()
;;   (let* ((defun-info (elpy-snippet-current-method-and-args))
;;          (class (nth 0 defun-info))
;;          (method (nth 1 defun-info))
;;          (args (nth 2 defun-info))
;;          (first-arg (nth 0 args))
;;          ;; (py-version-command " -c 'import sys ; print(sys.version_info.major)'")
;;          ;; Get the python version. Either 2 or 3
;;          (py-version-num (conda-env-python-major-version "python"))
;;          ;; (py-version-num (substring (shell-command-to-string
;;          ;;                             (concat elpy-rpc-python-command
;;          ;;                                     py-version-command))
;;          ;;                            0 1))
;;          )
;;     (if (string-match py-version-num "2")
;;         (format "(%s, %s).%s" class first-arg method)
;;       (format "().%s" method))))

;; (defun elpy-snippet-super-arguments ()
;;   "Return the argument list for the current method."
;;   (mapconcat (lambda (x) x)
;;              (cdr (nth 2 (elpy-snippet-current-method-and-args)))
;;              ", "))
