;;; nvp-cool.el --- cool compiler -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/compiler-tools
;; Last modified: <2019-02-26 04:49:34>
;; Created: 17 February 2017

;;; Commentary:
;; COOL compiler !!!
;;; Code:

(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'nvp-compiler)
(nvp-declare "" nvp-env-add nvp-env-path-add)

(defvar nvp-cool-root-path "/usr/class/cs143")

;; -------------------------------------------------------------------
;;; Utils

(eval-when-compile
  (defmacro nvp-cool--read-dir (&optional dir msg)
   `(ido-completing-read
     ,(or msg "Directory: ")
     (directory-files
      ,(if dir `(expand-file-name ,dir nvp-cool-root-path) 'nvp-cool-root-path)
      'full "[^.].*[^J]$"))))

;; -------------------------------------------------------------------
;;; Commands
(defvar-local cool--last-jump nil)

;; add headers/src to include path
(defun nvp-cool-setenv (dir)
  (interactive (list (nvp-cool--read-dir "include" "Path to add: ")))
  (nvp-env-add "CPLUS_INCLUDE_PATH" dir)
  (nvp-env-add "CPLUS_INCLUDE_PATH"
   (replace-regexp-in-string "include" "cool/src" dir)))

;; jump to source directory
(defun nvp-cool-jump-to-src (dir)
  (interactive
   (list (or (and (not current-prefix-arg) cool--last-jump)
             (setq cool--last-jump (nvp-cool--read-dir "include")))))
  (dired dir))

;; add executables to path
(defun nvp-cool-add-path (arg)
  (interactive "P")
  (let ((dir (if arg (ido-read-directory-name
                      "Directory to add to path: " "/")
               (expand-file-name "bin" nvp-cool-root-path))))
   (nvp-env-path-add dir)))

;;;###autoload
(defun nvp-cool-add-bindings ()
  (interactive)
  (nvp-bindings "c++-mode" 'cc-mode
    ("<f2> m j h" . nvp-cool-jump-to-src)
    ("<f2> m e c" . nvp-cool-setenv))
  (nvp-bindings "flex-mode" nil
    ("<f2> m j h" . nvp-cool-jump-to-src)
    ("<f2> m e c" . nvp-cool-setenv)))

(provide 'nvp-cool)
;;; nvp-cool.el ends here
