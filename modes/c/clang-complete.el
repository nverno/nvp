;;; clang-complete.el --- Manage .clang_complete file -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Maintainer: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/c-tools
;; Package-Requires: 
;; Created:  2 February 2017
;; Version:  0.0.1

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Do the .clang_complete file
;; - create with defaults
;; - update with new options

;; https://github.com/Rip-Rip/clang_complete/wiki
;; discusses making pre-compiled headers for clang_complete

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (defvar nvp-c-include-dirs)
  (defvar nvp-c++-include-dirs)
  (defvar nvp-clang-c-include-dirs)
  (defvar nvp-clang-c++-include-dirs))
(declare-function nvp-log "nvp-log")
(autoload 'nvp-env-merge "nvp-env")

(defvar clang-complete-default-defines '("DEBUG" "TEST")
  "Default symbols to define in .clang_comlete.")

;; get default system includes for c/c++
(defun clang-complete-default-includes (mode &optional system)
  (append
   (nvp-env-merge
    (if (eq mode 'c-mode) "C_INCLUDE_PATH" "CPLUS_INCLUDE_PATH") "CPATH")
   (and (not system)
        (bound-and-true-p c-tools-local-include-paths))
   (if (eq mode 'c-mode)
       (bound-and-true-p c-tools-clang-c-include-dirs)
     (bound-and-true-p c-tools-c++-include-dirs))))

;; generate list of defaults
(defsubst clang-complete-defaults (mode)
  (append
   (mapcar (lambda (s) (cons "-I" s)) (clang-complete-default-includes mode))
   (mapcar (lambda (s) (cons "-D" s)) clang-complete-default-defines)))

;; parse .clang-complete file options to alist
(defun clang-complete-parse-buffer ()
  (let (opts)
    (goto-char (point-min))
    (while (re-search-forward
            (nvp-concat (regexp-opt '("-I" "-D" "-include" "-std=" "-W") 'paren)
                        "\\(.*\\)$")
            nil 't)
      (push (cons (match-string 1) (match-string 2)) opts))
    opts))

;; merge options, sort, and concatenate
(defsubst clang-complete--merge (options)
  (mapconcat (lambda (s) (concat (car s) (cdr s)))
             (cl-sort
              (cl-remove-duplicates
               options :test (lambda (a b) (and (string= (car a) (car b))
                                           (string= (cdr a) (cdr b)))))
              (lambda (a b) (string< (car a) (car b))))
             "\n"))

;; read input string, split by whitespace, eg -DDEBUG -DTEST => ("-DDEBUG" "-DTEST")
;; buffer parser will do the rest
(defsubst clang-complete-read-input ()
  (let ((opts (read-from-minibuffer "Clang complete options: ")))
    (mapconcat 'identity (split-string opts) "\n")))

;; update/create .clang_complete file
;; with prefix, prompt from options to add.  Otherwise creates with defaults
;;;###autoload
(defun clang-complete-create-or-update (arg &optional mode options no-defaults)
  (interactive "P")
  (let ((mode (or mode major-mode))
        (init (not (file-exists-p ".clang_complete"))))
    (with-current-buffer (find-file-noselect ".clang_complete")
      (when arg
        (insert "\n")
        (insert (clang-complete-read-input)))
      (let ((opts (clang-complete-parse-buffer))
            (new-opts (nconc options
                             (and init (not no-defaults)
                                  (clang-complete-defaults mode)))))
        (erase-buffer)
        (insert (clang-complete--merge (nconc opts new-opts)))
        (save-buffer)
        (kill-buffer)))))

(provide 'clang-complete)
;;; clang-complete.el ends here
