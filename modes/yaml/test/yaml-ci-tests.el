;; -*- lexical-binding: t; -*-
(require 'nvp-macro)
(require 'nvp-proc)
(require 'nvp-yaml)
(require 'ert)

;; mocks for async validation function -- block until process completes
(defun nvp-yaml-ci-test-sentinel (p _m)
  (with-current-buffer (process-buffer p)
    (goto-char (point-min))
    (should (or (zerop (process-exit-status p))
                (looking-at-p "Valid!")
                (not (re-search-forward "error" nil t))))
    (kill-buffer (process-buffer p))))

(defun nvp-yaml-ci-validate (_type)
  (nvp:with-async-override 'nvp-yaml-lint-sentinel 'nvp-yaml-ci-test-sentinel
    (accept-process-output (nvp-yaml-validate) 2)))

(ert-deftest nvp-yaml-ci-codecov ()
  (let ((default-directory (expand-file-name (if (file-exists-p "test") "test" ""))))
    (with-current-buffer (find-file-noselect "codecov.yml")
      (should (eq 'codecov (nvp-yaml-type)))
      (nvp-yaml-ci-validate 'codecov))))

(ert-deftest nvp-yaml-ci-travis ()
  (let ((default-directory (expand-file-name (if (file-exists-p "test") "test" ""))))
   (with-current-buffer (find-file-noselect ".travis.yml")
     (should (eq 'travis (nvp-yaml-type)))
     (nvp-yaml-ci-validate 'travis))))

(ert-deftest nvp-yaml-ci-repo ()
  (should (string= "git@github.com:nverno/nvp" (nvp-yaml-project-url)))
  (should (string= "nverno/nvp" (nvp-yaml-project-repo)))
  (should (string= "nvp" (nvp-yaml-project-name))))

(ert-deftest nvp-yaml-ci-circleci ()
  (let ((default-directory (expand-file-name (if (file-exists-p "test") "test" ""))))
   (with-current-buffer (find-file-noselect ".circleci/config.yml")
     (should (eq 'circleci (nvp-yaml-type)))
     (nvp-yaml-ci-validate 'circleci))))
