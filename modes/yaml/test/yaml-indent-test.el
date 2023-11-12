;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'nvp-yaml-indent)

(defmacro yaml--should-indent (from to)
  `(with-temp-buffer
     (let ((nvp-yaml-indent-offset 2)
           (nvp-yaml-indent-cont-offset 2)
           (indent-line-function 'nvp-yaml-indent-line)
           (indent-region-function 'nvp-yaml-indent-region))
       (insert ,from)
       (indent-region (point-min) (point-max))
       (should (string= (buffer-substring-no-properties (point-min) (point-max))
                      ,to)))))

(ert-deftest yaml--test-indent-block-1 ()
  "Don't indent first block."
  (yaml--should-indent
   "
language: generic
sudo: false"
   "
language: generic
sudo: false"))

(ert-deftest yaml--test-indent-block-2 ()
  "Don't change indent of already indented blocks."
  (yaml--should-indent
   "
language: generic
  sudo: false"
   "
language: generic
  sudo: false"))

(ert-deftest yaml--test-indent-item-1 ()
  "Indent items relative to previous block."
  (yaml--should-indent
   "
script:
- make test"
   "
script:
  - make test"))

(ert-deftest yaml--test-indent-item-2 ()
  "Indent muliple items."
  (yaml--should-indent
   "
script:
- make test
- make all"
   "
script:
  - make test
  - make all"))

(ert-deftest yaml--test-indent-cont-1 ()
  "Indent continued bash item."
  (yaml--should-indent
   "
install:
- if [ ... ]; then
sudo ...
fi"
   "
install:
  - if [ ... ]; then
      sudo ...
    fi"))

(ert-deftest yaml--test-indent-cont-2 ()
  "Indent items + continued bash item."
  (yaml--should-indent
   "
install:
- if [ ... ]; then
sudo ...
fi
- blah blah
- if ; then
blah
fi
- blah
script:
- thing"
   "
install:
  - if [ ... ]; then
      sudo ...
    fi
  - blah blah
  - if ; then
      blah
    fi
  - blah
script:
  - thing"))

(defun yaml--run-tests ()
  (interactive)
  (if (featurep 'ert)
      (ert-run-tests-interactively "yaml--test")
    (message "cant run without ert.")))

(provide 'nvp-yaml-indent-tests)
