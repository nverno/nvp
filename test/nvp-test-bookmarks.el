;; -*- lexical-binding: t; -*-
(require 'nvp-test-helpers)
(require 'ert)
(require 'nvp-bookmark)

(ert-deftest nvp-bookmark-locate ()
  "Default searching for bookmark files"
  (let ((default-directory user-emacs-directory))
    (should (string= bookmark-default-file (nvp-bookmark-locate-file))))
  (let ((nvp-local-bookmark-file (expand-file-name "Makefile" nvp/nvp)))
    (should (string= (nvp-bookmark-locate-file)
                     nvp-local-bookmark-file))))

(provide 'nvp-test-bookmarks)
