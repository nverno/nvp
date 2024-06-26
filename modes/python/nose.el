;;; nose.el --- Easy Python test running in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2009 Jason Pellerin, Augie Fackler
;; Copyright (C) 2013-2015 Sylvain Benner

;; Created: 04 Apr 2009
;; Version: 0.3
;; Keywords: nose python testing

;; This file is NOT part of GNU Emacs.

;; Licensed under the same terms as Emacs.

;;; Commentary:
;; This gives a bunch of functions that handle running nosetests on a
;; particular buffer or part of a buffer.

;;; Installation

;; In your emacs config:
;;
;; (require 'nose)

;; This version is compatible with Windows.
;; It does not call directly the nosetests script. Instead it calls
;; python with an inline script to call nose.
;; It can launch test suites (require to install the nose fixes via
;; `easy_install nose-fixes`).
;; It is also compatible with virtualenv.

;; By default, the root of a project is found by looking for any of the files
;; '.projectile', 'setup.cfg', '.hg' and '.git'. You can add files to check for
;; to the file list:
;;
;;   (add-to-list 'nose-project-root-files "something")

;; or you can change the project root test to detect in some other way
;; whether a directory is the project root:
;;
;;   (setq nose-project-root-test (lambda (dirname) (equal dirname "foo")))

;; If you want dots as output, rather than the verbose output:
;;
;;   (defvar nose-use-verbose nil) ; default is t

;; Probably also want some keybindings:
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (local-set-key "\C-ca" 'nosetests-all)
;;             (local-set-key "\C-cm" 'nosetests-module)
;;             (local-set-key "\C-c." 'nosetests-one)
;;             (local-set-key "\C-cpa" 'nosetests-pdb-all)
;;             (local-set-key "\C-cpm" 'nosetests-pdb-module)
;;             (local-set-key "\C-cp." 'nosetests-pdb-one)))

(eval-when-compile (require 'cl-lib))

(defvar nose-project-root-files '(".projectile"
                                  "setup.cfg"
                                  ".hg"
                                  ".git"))
(defvar nose-project-root-test 'nose-project-root)
(defvar nose-use-verbose t)

(defun run-nose (&optional tests suite debug failed)
  "Run nosetests by calling python instead of nosetests script.

To be able to debug on Windows platform python output must be not buffered. For
more details:
http://pswinkels.blogspot.ca/2010/04/debugging-python-code-from-within-emacs.html."
  (let* ((nose (nosetests-nose-command))
         (where (nose-find-project-root))
         (args (concat (if debug "--pdb" "")
                       " "
                       (if failed "--failed" "")
                       " "
                       (if suite "--test-suite-func=load_tests" "")))
         (tnames (if tests tests "")))
    (if (not where)
        (error
         (format (concat "abort: nosemacs couldn't find a project root, "
                         "looked for any of %S")
                 nose-project-root-files)))
    (funcall (if debug
                 'pdb
               '(lambda (command)
                  (compilation-start command
                                     nil
                                     (lambda (mode) (concat "*nosetests*")))))
             (format
              (concat "%s "
                      (if nose-use-verbose "-v " "")
                      "%s -s -w \"%s\" -c \"%ssetup.cfg\" \"%s\"")
              nose args where where tnames))))

;; silence bc
(defvar python-shell-virtualenv-root)
(defun nosetests-nose-command ()
  (let ((nose "python -u -c \"import nose; nose.run()\""))
    (if python-shell-virtualenv-root
        (format "%s/bin/%s" python-shell-virtualenv-root nose)
      nose)))

(defun nose-py-testable ()
  (let* ((inner-obj (inner-testable))
         (outer (outer-testable))
         ;; elisp can't return multiple values
         (outer-def (car outer))
         (outer-obj (cdr outer)))
    (cond ((equal outer-def "def") outer-obj)
          ((equal inner-obj outer-obj) outer-obj)
          (t (format "%s.%s" outer-obj inner-obj)))))

(defun inner-testable ()
  (save-excursion
    (re-search-backward
     "^ \\{0,4\\}\\(class\\|def\\)[ \t]+\\([a-zA-Z0-9_]+\\)" nil t)
    (buffer-substring-no-properties (match-beginning 2) (match-end 2))))

(defun outer-testable ()
  (save-excursion
    (re-search-backward
     "^\\(class\\|def\\)[ \t]+\\([a-zA-Z0-9_]+\\)" nil t)
    (let ((result
           (buffer-substring-no-properties (match-beginning 2) (match-end 2))))

      (cons
       (buffer-substring-no-properties (match-beginning 1) (match-end 1))
       result))))

(defun nose-find-project-root (&optional dirname)
  (let ((dn
         (if dirname
             dirname
           (file-name-directory buffer-file-name))))
    (cond ((funcall nose-project-root-test dn) (expand-file-name dn))
          ((equal (expand-file-name dn) "/") nil)
          (t (nose-find-project-root
              (file-name-directory (directory-file-name dn)))))))

(defun nose-project-root (dirname)
  (cl-reduce (lambda (x y) (or x y))
             (mapcar (lambda (d) (member d (directory-files dirname)))
                     nose-project-root-files)))

;; -------------------------------------------------------------------
;;; Commands

(defun nosetests-all (&optional debug failed)
  "Run all nosetests.
If DEBUG is non-nil add '--pdb' argument.
If FAILED is non-nil add '--failed' argument."
  (interactive "P")
  (run-nose nil nil debug failed))

(defun nosetests-failed (&optional debug)
  "Run all nosetests with '--failed' non-nil.
If DEBUG is non-nil, run with '--pdb'."
  (interactive "P")
  (nosetests-all debug t))

(defun nosetests-pdb-all ()
  "Run all nosetests with '--pdb' enabled."
  (interactive)
  (nosetests-all t))

(defun nosetests-module (&optional debug)
  "Run nosetests (via eggs/bin/test) on current buffer.
If DEBUG is non-nil run with debugging enabled."
  (interactive "P")
  (run-nose buffer-file-name nil debug))

(defun nosetests-pdb-module ()
  "Run modules tests with debugging enabled."
  (interactive)
  (nosetests-module t))

(defun nosetests-suite (&optional debug)
  "Run nosetests (via eggs/bin/test) on current suite buffer.
If DEBUG is non-nil, run with debugging enabled."
  (interactive "P")
  (run-nose buffer-file-name t debug))

(defun nosetests-pdb-suite ()
  "Run nosetest suite with debugging enabled."
  (interactive)
  (nosetests-suite t))

(defun nosetests-one (&optional debug)
  "Run nosetests (via eggs/bin/test) on testable thing at point in current buffer.
If DEBUG is non-nil, run with debugging enabled."
  (interactive "P")
  (run-nose (format "%s:%s" buffer-file-name (nose-py-testable)) nil debug))

(defun nosetests-pdb-one ()
  "Debug a single test."
  (interactive)
  (nosetests-one t))

(provide 'nose)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nose.el ends here
