;;; bats-mode.el --- bats major mode -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/shell-tools
;; Last modified: <2019-03-31 00:53:02>
;; Package-Requires: 
;; Created:  1 January 2017

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

;; Emacs major-mode for bats source.
;;
;; See
;; - https://github.com/sstephenson/bats
;; - https://github.com/dougm/bats-mode

;;; Code:
(require 'sh-script)

(defvar bats-exe (executable-find "bats"))

(defvar bats-indent-offset 2)

;; -------------------------------------------------------------------
;;; Util

(eval-when-compile
  (defmacro re-opt (opts)
    `(concat "\\_<" (regexp-opt ,opts t) "\\_>"))

  (defmacro bats--beginning-of-test ()
    `(progn
       (end-of-line)
       (search-backward-regexp "^@test" nil 'move)))

  (defmacro bats--end-of-test ()
    `(progn
       (bats--beginning-of-test)
       (end-of-line)
       (up-list)
       (point)))

  ;; return name of current bats test
  (defmacro bats--current-test ()
    `(save-excursion
       (if (bats--beginning-of-test)
           (and (re-search-forward "^@test \"\\(.*?\\)\" *{" (line-end-position))
                (match-string 1))
         (user-error "Unable to find current @test"))))

  (defmacro bats--search (&optional back)
    `(condition-case nil
         (progn
           (forward-line ,(if back -1 1))
           (,(if back 're-search-backward 're-search-forward) "^@test")
           (beginning-of-line))
       (error (forward-line ,(if back 1 -1))))))

(defun bats-run (file &optional name)
  (let ((cmd (concat bats-exe " -t " file)))
    (compile (if name (concat (format "BATS_TEST_PATTERN='^%s$' " name) cmd) cmd))))

;; -------------------------------------------------------------------
;;; Commands

(defun bats-next-test ()
  (interactive)
  (bats--search))

(defun bats-previous-test ()
  (interactive)
  (bats--search 'back))

;; mark current test, keep marking successive tests when called
;; repeatedly
(defun bats-mark-test ()
  (interactive)
  (if (or (and (eq last-command this-command) (mark t))
          (and transient-mark-mode mark-active))
      (set-mark
       (save-excursion
         (goto-char (mark))
         (bats-next-test)
         (bats--end-of-test)))
    (let ((start (bats--beginning-of-test)))
      (when start
        (bats--end-of-test)
        (push-mark nil t t)
        (goto-char start)))))

(defun bats-run-current-test ()
  (interactive)
  (bats-run-current-file (bats--current-test)))

(defun bats-run-current-file (&optional name)
  (interactive)
  (if buffer-file-name 
      (bats-run buffer-file-name name)
    (user-error "Buffer not associated with a file")))

(defun bats-run-all ()
  (interactive)
  (bats-run "."))

;; -------------------------------------------------------------------
;;; Major-mode

;; Font-lock

(defvar bats-font-lock-keywords
  `(("\\(@test\\)" 1 font-lock-keyword-face)
    (,(re-opt '("load" "run" "skip")) 1 font-lock-function-name-face)))

;; Compilation

(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist 'bats t)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(bats . ("file \\([^ \t\r\n(]+\\), line \\([0-9]+\\)" 1 2)) t))

(defvar bats-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "M-N")     'bats-next-test)
    (define-key km (kbd "M-P")     'bats-previous-test)
    (define-key km (kbd "M-H")     'bats-mark-test)
    (define-key km (kbd "C-c C-b") 'bats-run-current-file)
    (define-key km (kbd "C-c C-a") 'bats-run-all)
    (define-key km (kbd "C-c C-c") 'bats-run-current-test)
    km))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bats\\'" . bats-mode))

;;;###autoload
(define-derived-mode bats-mode sh-mode "Bats"
  "Major mode for Bats source files.

Commands: 
\\{bats-mode-map}"
  (setq-local sh-shell "bash")
  (setq-local sh-basic-offset bats-indent-offset)
  (font-lock-add-keywords 'bats-mode bats-font-lock-keywords))

(provide 'bats-mode)
;;; bats-mode.el ends here
