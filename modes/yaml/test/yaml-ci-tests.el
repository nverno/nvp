;; -*- lexical-binding: t; -*-
(require 'nvp-macro)
(require 'nvp-yaml)
(require 'ert)

(defun nvp-yaml-ci-test-sentinel (p m)
  (with-current-buffer (process-buffer p)
    (goto-char (point-min))
    (or (zerop (process-exit-status p))
        (not (re-search-forward "error" nil t)))))

(defun nvp-yaml-ci-validate ()
  (nvp-with-async-override 'nvp-yaml-lint-sentinel 'nvp-yaml-ci-test-sentinel
    (nvp-yaml-validate)))

(ert-deftest nvp-yaml-ci-circleci ()
  (with-current-buffer (find-file-noselect (expand-file-name ".circleci/config.yml"))
    (should (eq 'circleci (nvp-yaml-type)))
    (should-not (nvp-yaml-ci-validate))))

(ert-deftest nvp-yaml-ci-codecov ()
  (with-current-buffer (find-file-noselect (expand-file-name "codecov.yml"))
    (should (eq 'codecov (nvp-yaml-type)))
    (should-not (nvp-yaml-ci-validate))
    (should (buffer-live-p (get-buffer "*lint-codecov*")))))

(ert-deftest nvp-yaml-ci-travis ()
  (with-current-buffer (find-file-noselect (expand-file-name ".travis.yml"))
    (should (eq 'travis (nvp-yaml-type)))))

(ert-deftest nvp-yaml-ci-repo ()
  (should (string= "https://github.com/nverno/nvp" (nvp-yaml-project-url)))
  (should (string= "nverno/nvp" (nvp-yaml-project-repo)))
  (should (string= "nvp" (nvp-yaml-project-name))))
