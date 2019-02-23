;;; nvp-bookmark.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-22 18:10:28>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 24 November 2016

;;; Commentary:
;; not used
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'bookmark)

(declare-function bmk-to-bmk-handler "bmk-to-bmk")
(declare-function bmk-to-bmk-make-record "bmk-to-bmk")

;;;###autoload
(defun nvp-bookmark-local (file)
  (interactive
   (list (or
          (bound-and-true-p file)
          (and current-prefix-arg
               (read-file-name "Location of bookmark file: "))
          (and (bound-and-true-p local-bookmarks)
               (expand-file-name
                local-bookmarks
                (locate-dominating-file default-directory
                                        dir-locals-file)))
          bookmark-default-file)))
  (message "Current bookmarks file is: %s" file)
  (when (not (string= bookmark-default-file file))
    (bmk-to-bmk-handler (bmk-to-bmk-make-record file)))
  (call-interactively 'bookmark-bmenu-list))

(provide 'nvp-bookmark)
;;; nvp-bookmark.el ends here
