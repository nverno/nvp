(require 'nvp-yaml)
(require 'ert)

(ert-deftest nvp-yaml-ci-circleci ()
  (with-current-buffer (find-file-noselect (expand-file-name ".circleci/config.yml"))
    (should (eq 'circleci (nvp-yaml-ci-type)))))

(ert-deftest nvp-yaml-ci-travis ()
  (with-current-buffer (find-file-noselect (expand-file-name ".travis.yml"))
    (should (eq 'travis (nvp-yaml-ci-type)))))

(ert-deftest nvp-yaml-ci-repo ()
  (should (string= "https://github.com/nverno/nvp" (nvp-yaml-project-url)))
  (should (string= "nverno/nvp" (nvp-yaml-project-repo)))
  (should (string= "nvp" (nvp-yaml-project-name))))
