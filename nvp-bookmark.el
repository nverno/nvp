;;; nvp-bookmark.el ---  -*- lexical-binding: t; -*-

;; Last modified: <2019-03-28 18:05:00>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 24 November 2016

;;; Commentary:
;; TODO: incorporate bmk-to-bmk if there is anything useful
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'nvp)
(require 'bookmark)
(nvp-declare "bmk-to-bmk" bmk-to-bmk-make-record bmk-to-bmk-handler)

;;;###autoload
(defun nvp-bookmark-local (file)
  (interactive
   (list (or (and current-prefix-arg
                  (read-file-name "Location of bookmark file: "))
             (and (bound-and-true-p nvp-local-bookmark-file)
                  (expand-file-name
                   nvp-local-bookmark-file
                   (locate-dominating-file default-directory dir-locals-file)))
             bookmark-default-file)))
  (message "Using bookmark file: %s" file)
  (when (not (string= bookmark-default-file file))
    (bmk-to-bmk-handler (bmk-to-bmk-make-record file)))
  (call-interactively 'bookmark-bmenu-list))

(provide 'nvp-bookmark)
;;; nvp-bookmark.el ends here
