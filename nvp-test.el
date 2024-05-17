;;; nvp-test.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; TODO: completely refactor
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls)
(nvp:auto "projectile" projectile-root-top-down)
(nvp:auto "nvp-project" nvp-project-locate-root)

;; called when visiting new test buffer, passed name of matching source file
(defvar nvp-test-init-function #'ignore)

;; called when visiting test buffer with no arguments
(defvar nvp-test-init-buffer-function #'ignore)

;; function to run unit file. Passed current buffer-file-name
(defvar nvp-test-run-unit-function #'ignore)

;; filter test files
(defvar nvp-test-prefixes '("test-" "test_" "t-" "t_" "Test"))
(defvar nvp-test-suffixes '("-test" "-tests" "_test"))
(defvar nvp-test-extension-re nil)

;;; XXX: remove
(defvar-local nvp-project--root '(".git" ".projectile" "test" "tests"))
(defvar-local nvp-project--test-re ".*tests?")
(defvar-local nvp-project--test-dir '("test" "tests" "t"))
(defvar-local nvp-project--test-fmt "test-%s")

;;;###autoload
(defun nvp-ert-run-tests ()
  "Run ert tests.
With prefix ARG, prompt for selector."
  (interactive)
  (if (not (require 'ert nil t))
      (user-error "`ert' must be loaded to run this function")
    (eval-buffer (current-buffer))
    (call-interactively #'ert-run-tests-interactively)))

;; -------------------------------------------------------------------
;;; Find test files 

;; create test directory in project root or local root if doesn't exist
(defsubst nvp-test-create-test-dir (root &optional dir)
  (let ((test-dir
         (expand-file-name
          (or (car-safe (or dir nvp-project--test-dir)) "test") root)))
    (make-directory test-dir :create-parents)
    test-dir))

;; possible test directories (choose first found):
;; - test tests t
(defun nvp-test-dir (&optional local create test-dirs)
  (let* ((default-directory (nvp-project-locate-root local nvp-project--root))
         (test-dir (cl-find-if (lambda (d)
                                 (and (file-exists-p d)
                                      (file-directory-p d)
                                      d))
                               (or test-dirs nvp-project--test-dir))))
    (if (and create (not test-dir))
        (nvp-test-create-test-dir
         default-directory (or test-dirs nvp-project--test-dir))
      (and test-dir (expand-file-name test-dir)))))

;; list of test files in project test folder
(defsubst nvp-test--test-files (dir &optional test-re)
  (directory-files dir t (or test-re nvp-project--test-re)))

;; provide a default name for either a new test or history of buffer-local
;; previous test files
(defvar-local nvp-test-file-history nil)
(defsubst nvp-test-default-test-name (file)
  (or (car-safe nvp-test-file-history)
      (format (or (bound-and-true-p nvp-project--test-fmt) "test-%s")
              (file-name-nondirectory file))))

;; non-nil if FILE is a test file matching SOURCE-FILE, use
;; basename for SOURCE-FILE
(defsubst nvp-test-matching-test-p (file source-file &optional prefixes suffixes)
  (cl-member
   (file-name-sans-extension (file-name-nondirectory file))
   (append
    (mapcar (lambda (prefix) (concat prefix source-file))
            (or prefixes nvp-test-prefixes))
    (mapcar (lambda (suffix) (concat source-file suffix))
            (or suffixes nvp-test-suffixes)))
   :test 'string=))

;; non-nil if current buffer is a test file
(defsubst nvp-test-file-p ()
  (and (cl-member (nvp:path 'ds) nvp-project--test-dir :test 'string=)
       (let ((buff-name (nvp:path 'bfse)))
         (or (cl-some (lambda (prefix)
                        (string-prefix-p (regexp-quote prefix) buff-name))
                      nvp-test-prefixes)
             (cl-some (lambda (suffix)
                        (string-suffix-p (regexp-quote suffix) buff-name))
                      nvp-test-suffixes)))))

;; return test-file matching current source-file if it exists
(defun nvp-test-find-matching-test (source-file test-dir &optional prefixes suffixes)
  (let* ((default-directory test-dir)
         (file-ext (or nvp-test-extension-re (file-name-extension source-file)))
         (files (directory-files
                 test-dir t (concat "^[^.].*" (regexp-quote file-ext) "$")))
         (basename (file-name-nondirectory (file-name-sans-extension source-file))))
    (cl-find-if (lambda (file)
                  (nvp-test-matching-test-p file basename prefixes suffixes))
                files)))

;; If test file found that matches buffer-file-name, return that,
;; otherwise prompt with completing-read 
(defun nvp-test-find-or-read-matching-test (source-file test-dir &optional
                                                          prefixes suffixes)
  (let* ((default-directory test-dir)
         (test-file
          (or (nvp-test-find-matching-test source-file test-dir prefixes suffixes)
              ;; read name of new test
              (expand-file-name
               (funcall-interactively
                'ido-completing-read "Name of (new) test file: "
                (directory-files test-dir nil "^[^.]")
                nil nil (nvp-test-default-test-name source-file))
               default-directory))))
    ;; keep history
    (cl-pushnew (file-name-nondirectory test-file) nvp-test-file-history)
    test-file))

(defmacro nvp-with-test (&optional local create no-test prefixes suffixes &rest body)
  "Do BODY in project test file, prompting if more than one is found.
Do NO-TEST if no tests are found, default to user-error."
  (declare (indent defun))
  `(let ((test-dir (nvp-test-dir ,local ,create)))
     (unless test-dir
       (user-error "No test directory found."))
     (let* ((source-file (buffer-file-name))
            (test-file
             (nvp-test-find-or-read-matching-test source-file test-dir
                                                    ,prefixes ,suffixes))
            (new-file (not (file-exists-p test-file)))
            (init-function nvp-test-init-function)
            (buffer-function nvp-test-init-buffer-function))
       (if (not test-file)
           ,(or no-test '(user-error "No test files found."))
         (with-current-buffer (find-file-noselect test-file)
           (and new-file (funcall-interactively init-function source-file))
           (funcall-interactively buffer-function)
           ,@body)))))

;; -------------------------------------------------------------------
;;; Commands 

;; jump to test file associated with current source file, create one if 
;; CREATE is non-nil
;;;###autoload
(defun nvp-test-jump-to-test (create)
  (interactive "P")
  (nvp-with-test 'local create nil nil nil (pop-to-buffer (current-buffer))))

;; Run or create unit tests. If in test file, call `nvp-test-run-unit-function'
;; with `buffer-file-name'. If in source file, run associated test file,
;; or create one if necessary. Test runner function is passed the test file
;; name and prefix arg, ARG
;;;###autoload
(defun nvp-test-run-unit-test ()
  (interactive)
  (let ((file (if (nvp-test-file-p) (buffer-file-name)
                (nvp-test-find-matching-test
                 (buffer-file-name) (nvp-test-dir 'local)))))
    (if file
        (funcall-interactively nvp-test-run-unit-function file)
      (and (y-or-n-p "No matching unit test found, jump to new one?")
           (nvp-test-jump-to-test 'create)))))

(provide 'nvp-test)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-test.el ends here
