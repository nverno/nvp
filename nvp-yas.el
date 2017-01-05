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
  (require 'nvp-local nil t)
  (require 'nvp-macro)
  (defvar nvp/snippet))
(require 'yasnippet)
(autoload 'string-trim "subr-x")

(defvar-local nvp-snippet-dir nil)

;; compile snippets when installing mode
(defun nvp-yas-snippet-install ()
  (let ((yas-snippet-dirs nvp/snippet))
    (yas-recompile-all)))

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

;; -------------------------------------------------------------------
;;; Snippet helpers

(defsubst nvp-yas-with-comment (str)
  (let ((comment (if (or (memq major-mode
                               '(python-mode c-mode c++-mode flex-mode))
                         (string= comment-end ""))
                     comment-start
                   (concat comment-start comment-start))))
    (format "%s%s%s" comment str comment-end)))

;; current indentation
(defsubst nvp-yas-indent ()
  (save-excursion
    (back-to-indentation)
    (current-column)))

;; create comment string of length LENGTH, accounting for
;; multi-character comments by recycling the second char
(defalias 'yas-comment-string 'nvp-yas-comment)
(defsubst nvp-yas-comment (length)
  (ignore-errors
    (let* ((comment (string-trim comment-start))
           (cont (if (> (length comment) 1)
                     (substring comment 1 2) comment)))
      (concat comment (make-string (max 0 (- length (length comment)))
                                   (string-to-char cont))))))

(defsubst nvp-yas-header (char &optional extra max)
  (let ((sw (string-width yas-text)))
    (make-string (if extra (min max (+ sw extra)) sw) char)))

;; add padding to `yas-text'
(defsubst nvp-yas-pad (char padmin padmax)
  (let* ((sw (string-width yas-text))
         (extra (max padmin (- padmax sw))))
    (make-string (/ (max 0 extra) 2) char)))

;; continuation comment: if `comment-end' is defined, just make a
;; blank string
(defsubst nvp-yas-comment-cont (length)
  (if (and comment-end (not (string= "" comment-end)))
      (make-string length ? )
    (nvp-yas-comment length)))

;; `comment-end' or default to ""
(defsubst nvp-yas-comment-end ()
  (or (bound-and-true-p comment-end) ""))

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
    ;; don't clobber current snippet if in snippet-mode
    (switch-to-buffer-other-window (if (eq major-mode 'snippet-mode)
                                       "**new snippet*"
                                     "*new snippet*"))
    (erase-buffer)
    (kill-all-local-variables)
    (snippet-mode)
    (yas-minor-mode 1)
    (yas-expand-snippet yas-new-snippet-default)

    ;; reload / compile after save
    (if arg
        (add-hook 'after-save-hook (lambda () (nvp-yas-snippet-reload nil t))
                  nil 'local)
      (add-hook 'after-save-hook 'nvp-yas-snippet-reload nil 'local))))

(provide 'nvp-yas)
;;; nvp-yas.el ends here
