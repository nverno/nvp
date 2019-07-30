;;; nvp-boost. ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))

;; -------------------------------------------------------------------
;;; Util

(defvar nvp-boost--version nil)

(eval-when-compile
  (defmacro nvp-boost--version-string (arg)
    `(or nvp-boost--version
         (setq nvp-boost--version
               (mapconcat
                'number-to-string
                (nvp-boost--version
                 (and ,arg (read-directory-name "Boost root directory: ")))
                "_")))))

;; get current boost version
(defun nvp-boost--version (&optional boost-root)
  (let ((file (expand-file-name "version.hpp" (or boost-root
                                                  (getenv "BOOST_ROOT")
                                                  "/usr/include/boost"))))
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (when (re-search-forward "#define BOOST_VERSION \\([0-9]+\\)")
          (let ((num (string-to-number (match-string 1))))
            (prog1 (list (/ num 100000) (% (/ num 100) 1000) (% num 100))
              (kill-buffer (current-buffer)))))))))

;; -------------------------------------------------------------------
;;; Commands

;;;###autoload
(defun nvp-boost-version (arg)
  (interactive "P")
  (message "Boost version: %s" (nvp-boost--version-string arg)))

;;;###autoload
(defun nvp-boost-lookup-help (arg)
  (interactive "P")
  (browse-url (concat "www.boost.org/doc/libs/" (nvp-boost--version-string arg))))

(provide 'nvp-boost)
;;; nvp-boost.el ends here
