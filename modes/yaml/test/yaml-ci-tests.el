;; -*- lexical-binding: t; -*-
(eval-when-compile (require 'nvp-macro))
(require 'nvp-proc)
(require 'nvp-yaml)
(require 'ert)

;; mocks for async validation function -- block until process completes
(defun nvp-yaml-ci-test-sentinel (p _m)
  (with-current-buffer (process-buffer p)
    (goto-char (point-min))
    (should (or (zerop (process-exit-status p))
                (not (re-search-forward "error" nil t))))
    (kill-buffer (process-buffer p))))

(defun nvp-yaml-ci-validate (_type)
  (nvp-with-async-override 'nvp-yaml-lint-sentinel 'nvp-yaml-ci-test-sentinel
    (accept-process-output (nvp-yaml-validate) 0.2)))

(ert-deftest nvp-yaml-ci-circleci ()
  (with-current-buffer
      (find-file-noselect (expand-file-name ".circleci/config.yml" "test"))
    (should (eq 'circleci (nvp-yaml-type)))
    (nvp-yaml-ci-validate 'circleci)))

(ert-deftest nvp-yaml-ci-codecov ()
  (with-current-buffer (find-file-noselect (expand-file-name "codecov.yml" "test"))
    (should (eq 'codecov (nvp-yaml-type)))
    (nvp-yaml-ci-validate 'codecov)))

(ert-deftest nvp-yaml-ci-travis ()
  (with-current-buffer (find-file-noselect (expand-file-name ".travis.yml" "test"))
    (should (eq 'travis (nvp-yaml-type)))
    (nvp-yaml-ci-validate 'travis)))

(ert-deftest nvp-yaml-ci-repo ()
  (should (string= "https://github.com/nverno/nvp" (nvp-yaml-project-url)))
  (should (string= "nverno/nvp" (nvp-yaml-project-repo)))
  (should (string= "nvp" (nvp-yaml-project-name))))
