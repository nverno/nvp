;;; bats-mode.el --- bats major mode -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/shell-tools
;; Last modified: <2019-05-01.00>
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
;;
;; Emacs major-mode for bats source.
;;
;; See
;; - https://github.com/sstephenson/bats
;; - modified from https://github.com/dougm/bats-mode
;;
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'compile))
(require 'sh-script)
(require 'nvp)
(require 'nvp-sh)

(defvar bats-exe (nvp:program "bats") "Bats executable.")

(defvar bats-indent-offset sh-basic-offset "Bats indentation offset.")

(defvar bats-check-program
  (nvp:program "batscheck.sh" :path (expand-file-name "sh" nvp/bin)))

(defvar bats-function-re
  (nvp:concat
   "^\\s-*\\(?:"
   ;; function FOO()
   "function\\s-+\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*\\(?:()\\)?"
   "\\|"
   ;; FOO()
   "\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*()"
   "\\|"
   ;; bats tests
   "@test"
   "\\)"))

(defun bats-current-test ()
  "Find current bats test."
  (let (test-name)
    (save-excursion
      (end-of-line)
      (unless (search-backward-regexp "^@test \"\\(.*?\\)\" {" nil t)
        (error "Unable to find a @test"))
      (setq test-name (match-string 1)))
    test-name))

;; -------------------------------------------------------------------
;;; Commands

;; mark current test, keep marking successive tests when called
;; repeatedly
;; (defun bats-mark-test ()
;;   (interactive)
;;   (if (or (and (eq last-command this-command) (mark t))
;;           (and transient-mark-mode mark-active))
;;       (set-mark
;;        (save-excursion
;;          (goto-char (mark))
;;          (bats-next-test)
;;          (bats--end-of-test)))
;;     (let ((start (bats--beginning-of-test)))
;;       (when start
;;         (bats--end-of-test)
;;         (push-mark nil t t)
;;         (goto-char start)))))

;; Compilation

(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist 'bats t)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(bats . ("file \\([^ \t\r\n(]+\\), line \\([0-9]+\\)" 1 2)) t))

;; just highlight checkmarks
(defun bats-compilation-filter ()
  (save-excursion
    (forward-line 0)
    (let ((end (point)))
      (goto-char compilation-filter-start)
      (forward-line 0)
      (when (< (point) end)
        (setq end (copy-marker end))
        (while (search-forward "✓" end 1)
          (replace-match
           (propertize (match-string 0) 'face 'bold 'font-lock-face 'compilation-info)
           t t))))))

;; align results by checkmarks
(defun bats-compilation-finish (&rest _)
  (let ((inhibit-read-only t))
   (align-regexp (point-min) (point-max) "\\(\\s-+\\)✓" 1)))

;; FIXME: Doesn't look like there is a BATS_TEST_PATTERN anymore
(defun bats-run (file &optional name)
  (let ((cmd (concat bats-exe " -p " file)))
    (with-current-buffer 
        (compile (if name
                     (concat (format "BATS_TEST_PATTERN='^%s$' " name) cmd)
                   cmd))
      (add-hook 'compilation-filter-hook #'bats-compilation-filter nil t)
      (add-hook 'compilation-finish-functions #'bats-compilation-finish nil t))))

(defun bats-run-current-test ()
  (interactive)
  (bats-run-current-file (bats-current-test)))

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
    (,(nvp:re-opt '("load" "run" "skip")) 1 font-lock-function-name-face)
    (,(nvp:re-opt '("setup" "teardown")) 1 'nvp-special-type-face)))

(defvar bats-mode-map
  (let ((km (make-sparse-keymap)))
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
  (font-lock-add-keywords 'bats-mode bats-font-lock-keywords)
  (setq-local nvp-sh-function-re bats-function-re)
  (setq-local beginning-of-defun-function #'nvp-sh-beginning-of-defun)
  (setq-local end-of-defun-function #'nvp-sh-end-of-defun)
  (setq-local compile-command '(concat bats-check-program " " (buffer-file-name))))

(provide 'bats-mode)
;;; bats-mode.el ends here
