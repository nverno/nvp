;;; nvp-snippet.el --- snippet mode -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-07 14:32:12>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created:  7 February 2019

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
  (require 'cl-lib)
  (require 'subr-x)
  (require 'nvp-macro)
  (nvp-local-vars))
(require 'nvp)
(require 'yasnippet)

;; ------------------------------------------------------------
;;; Jump to new snippet

;; #<marker at 95724 in yasnippet.el>
;;;###autoload
(defun nvp-jump-to-new-snippet (arg)
  (interactive "P")
  (when (not (fboundp 'yas-new-snippet))
    (require 'yasnippet))
  (let* ((mm (symbol-name major-mode))
         (default-directory (or (and arg (equal arg '(16)) nvp/snippet)
                                nvp-snippet-dir
                                (expand-file-name mm nvp/snippet)))
         (yas-selected-text (or yas-selected-text
                                (and (region-active-p)
                                     (buffer-substring-no-properties
                                      (region-beginning) (region-end))))))
    (when (not (file-exists-p default-directory))
      (make-directory default-directory))
    ;; with prefix dired the snippet directory
    (if (equal arg '(4)) (dired default-directory)
      (switch-to-buffer-other-window (generate-new-buffer "*snippet*"))
      (erase-buffer)
      (kill-all-local-variables)
      (snippet-mode)
      (yas-expand-snippet yas-new-snippet-default)
      ;; reload / compile after save
      ;; (add-hook 'after-save-hook 'nvp-snippet-reload nil 'local)
      )))

;; replace macros in snippet-mode with expansions
;; #<marker at 21701 in macrostep.el>
;; Reload directory after saving
(defun nvp-snippet-reload (&optional dir compile)
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

;; de/in-crement snippet expansion numbers in selected region
(defun nvp-snippet-increment-count (start end)
  (interactive "r")
  (goto-char start)
  (while (re-search-forward "\$\{\\([[:digit:]]\\):" end 'move)
    (replace-match (number-to-string
                    (+ (if current-prefix-arg -1 1)
                       (string-to-number (match-string 1))))
                   nil nil nil 1)))

;;;###autoload
(defun nvp-snippet-help-at-point ()
  (interactive)
  (browse-url "https://joaotavora.github.io/yasnippet/snippet-expansion.html"))

(provide 'nvp-snippet)
;;; nvp-snippet.el ends here
