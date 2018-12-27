;;; nvp-yas ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 20 December 2016

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
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (nvp-local-vars))
(require 'yasnippet)
(declare-function company-abort "company")
(declare-function company-complete-common "company")
(autoload 'string-trim "subr-x")

(defvar-local nvp-snippet-dir nil)

;; -------------------------------------------------------------------
;;; Setup 

;; compile snippets when installing mode
(defun nvp-yas-snippet-install ()
  (let ((yas-snippet-dirs nvp/snippet))
    (yas-recompile-all)))

;; -------------------------------------------------------------------
;;; Snippet helpers

;;; Comments
(autoload 'nvp-comment-string "nvp-comment")
(define-obsolete-function-alias 'nvp-yas-with-comment 'nvp-comment-string)

(autoload 'nvp-comment-make-comment "nvp-comment")
(define-obsolete-function-alias 'nvp-yas-comment 'nvp-comment-make-comment)
(defalias 'yas-comment-string 'nvp-yas-comment)

(autoload 'nvp-comment-continued "nvp-comment")
(define-obsolete-function-alias 'nvp-yas-comment-cont 'nvp-comment-continued)

;; `comment-end' or default to ""
(autoload 'nvp-comment-end "nvp-comment")
(define-obsolete-function-alias 'nvp-yas-comment-end 'nvp-comment-end)

;; trimmed filename
(defsubst nvp-yas-bfn ()
  (nvp-bfn))

;; directory name
(defsubst nvp-yas-dfn ()
  (nvp-dfn))

;; current indentation
(defsubst nvp-yas-indent ()
  (current-indentation))

(defsubst nvp-yas-header (char &optional extra max)
  (let ((sw (string-width yas-text)))
    (make-string (if extra (min max (+ sw extra)) sw) char)))

;; add padding to `yas-text'
(defsubst nvp-yas-pad (char padmin padmax)
  (let* ((sw (string-width yas-text))
         (extra (max padmin (- padmax sw))))
    (make-string (/ (max 0 extra) 2) char)))

;; fill after yas-text with CHAR until PADMAX
(defsubst nvp-yas-pad-right (char padmax)
  (make-string (max 0 (- padmax (string-width yas-text))) char))

;;--- Args -----------------------------------------------------------

;; split argument STR by SEPS, eg. "a,b" => '("a" "b"). Strings are trimmed and
;; nulls are dropped.
;; if DEFAULTS is non-nil, split by "=" as well, eg.
;; "a,b=1," => '("a" ("b" "1"))
(defun nvp-yas-split-args (str &optional seps defaults)
  (let ((args (split-string str (or seps "[ \t]*,[ \t]*") t " ")))
    (if defaults
        (mapcar (lambda (s)
                  (let ((defs (split-string s "[ \t]*=[ \t]*" t " ")))
                    (if (= (length defs) 1) (car defs) defs)))
                args)
      args)))

;; get variable name from string of form "i = 1" or "int i = 1"
(defun nvp-yas-varname (str)
  (if (< (length str) 1)
      ""
   (let* ((str (car (split-string str "=" t " ")))
          (strs (split-string str nil t " ")))
     (or (cadr strs) (car strs)))))

;; -------------------------------------------------------------------
;;; Commands 

;; de/in-crement snippet expansion numbers in selected region
(defun nvp-yas-increment-count (start end)
  (interactive "r")
  (goto-char start)
  (while (re-search-forward "\$\{\\([[:digit:]]\\):" end 'move)
    (replace-match (number-to-string
                    (+ (if current-prefix-arg -1 1)
                       (string-to-number (match-string 1))))
                   nil nil nil 1)))

;; Initialize a directory of snippets.
(defun nvp-yas-snippets-initialize (dir)
  (interactive)
  (when (and (fboundp 'yas-load-directory)
	     (file-directory-p dir))
    (yas-load-directory (expand-file-name dir nvp/snippet))))

(defun nvp-yas-goto-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas-active-snippets)))
	 (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

(defun nvp-yas-goto-start-of-active-field ()
  (interactive)
  (let* ((snippet (car-safe (yas-active-snippets)))
	 (position (yas--field-start
                    (yas--snippet-active-field snippet))))
    (if (= (point) position)
	(move-beginning-of-line 1)
      (goto-char position))))

;; Expand yasnippet if possible, otherwise complete.
;; FIXME: remove?
(defun nvp-yas-company-or-snippet ()
  (interactive)
  (if (car (yas--templates-for-key-at-point))
      (progn
        (company-abort)
        (yas-expand))
    (company-complete-common)))

(defun nvp-yas-snippet-help ()
  (interactive)
  (browse-url "https://joaotavora.github.io/yasnippet/snippet-expansion.html"))

;; reload all `yas-snippet-dirs'
;;;###autoload
(defun nvp-yas-reload-all ()
  (interactive)
  (cl-loop for dir in yas-snippet-dirs
     do (yas-load-directory dir)))

;; ------------------------------------------------------------
;;;  Snippet-mode

;; replace macros in snippet-mode with expansions
;; #<marker at 21701 in macrostep.el>
;; Reload directory after saving
(defun nvp-yas-snippet-reload (&optional dir compile)
  (let* ((ddir (file-name-directory
                (expand-file-name default-directory)))
         (parent (file-name-nondirectory
                  (directory-file-name 
                   (file-name-directory (directory-file-name ddir)))))
         ;; if in yas group folder, parent one extra directory up
         (ddir (if (not (string= "snippets" parent))
                   (file-name-directory
                    (directory-file-name
                     (file-name-directory ddir)))
                 ddir)))
    (when compile
      (yas-compile-directory (or dir ddir)))
    (yas-load-directory
     (or dir ddir))))
;; (yas-recompile-all)
;; (yas-reload-all)

;; #<marker at 95724 in yasnippet.el>
;;;###autoload
(defun nvp-jump-to-new-snippet (arg)
  (interactive "P")
  (when (not (fboundp 'yas-new-snippet))
    (require 'yasnippet))
  (let* ((mm (symbol-name major-mode))
         (default-directory (or nvp-snippet-dir
                                (expand-file-name mm nvp/snippet)))
         (yas-selected-text (or yas-selected-text
                                (and (region-active-p)
                                     (buffer-substring-no-properties
                                      (region-beginning) (region-end))))))
    (when (not (file-exists-p default-directory))
      (make-directory default-directory))
    ;; with prefix dired the snippet directory
    (if arg (dired default-directory)
      ;; don't clobber current snippet if in snippet-mode
      (switch-to-buffer-other-window (and (eq major-mode 'snippet-mode)
                                          (rename-uniquely)))
      (erase-buffer)
      (kill-all-local-variables)
      (snippet-mode)
      (yas-minor-mode 1)
      (yas-expand-snippet yas-new-snippet-default)
      
      ;; reload / compile after save
      (add-hook 'after-save-hook 'nvp-yas-snippet-reload nil 'local))))

(provide 'nvp-yas)
;;; nvp-yas.el ends here
