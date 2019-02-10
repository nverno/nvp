;;; nvp-snippet.el --- snippet mode -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-10 05:40:05>
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
(declare-function nvp-read-mode "nvp-read")

;; ------------------------------------------------------------
;;; Jump to new snippet

;; #<marker at 95724 in yasnippet.el>
;;;###autoload
(defun nvp-jump-to-new-snippet (mode snippet-dir &optional do-dired text)
  "Jump to a new snippet for MODE in snippet SNIPPET-DIR (creating if necessary).
If DO-DIRED is non-nil, `dired' that directory instead of creating snippet.
If TEXT is non-nil use as `yas-selected-text'."
  (interactive
   (let ((mode-name (if (equal current-prefix-arg '(64)) (nvp-read-mode)
                      (symbol-name major-mode))))
     (list mode-name
           (if (or (equal current-prefix-arg '(16)) (not nvp-snippet-dir))
               (expand-file-name mode-name nvp/snippet)
             nvp-snippet-dir)
           (and (equal current-prefix-arg '(4)) 'do-dired)
           (or yas-selected-text
               (and (region-active-p)
                    (buffer-substring-no-properties
                     (region-beginning) (region-end)))))))
  (and (symbolp mode) (setq mode (symbol-name mode)))
  (unless (file-exists-p snippet-dir)
    (make-directory snippet-dir))
  ;; with prefix dired the snippet directory
  (if do-dired (dired snippet-dir)
    (switch-to-buffer-other-window (generate-new-buffer "*snippet*"))
    (snippet-mode)
    (yas-minor-mode)
    (setq-local default-directory snippet-dir)
    (yas-expand-snippet
     yas-new-snippet-default nil nil `((yas-selected-text ,text)))
    ;; reload / compile after save
    ;; (add-hook 'after-save-hook 'nvp-snippet-reload nil 'local)
    ))

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
