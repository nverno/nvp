;;; nvp-mark --- 

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 29 November 2016

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
  (require 'subr-x))
(autoload 'find-library-name "find-func")

;;;###autoload
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; thing-at-point 'marker
(defun nvp-mark-bounds-of-marker-at-point ()
  (save-excursion
    (skip-chars-backward "^#" (line-beginning-position))
    (when (eq ?< (char-after (point)))
      (let ((start (1- (point)))
            (end (progn
                   (skip-chars-forward "^>" (line-end-position))
                   (1+ (point)))))
        (cons start end)))))

(put 'marker 'bounds-of-thing-at-point
     'nvp-mark-bounds-of-marker-at-point)

;; jump to marker at point
;; markers: #<marker at [point or fn-name] in [library/relative filename]>
;;;###autoload
(defun nvp-mark-goto-marker-at-point ()
  (interactive)
  (when-let ((bnds (bounds-of-thing-at-point 'marker)))
            (save-excursion
              (goto-char (car bnds))
              (save-match-data
                (when (re-search-forward
                       "<marker at \\([^ \t\n]+\\) in \\([-a-zA-Z0-9.]+\\)>"
                       (line-end-position) t)
                  (let* ((fn-or-place (match-string-no-properties 1))
                         (place (string-to-number fn-or-place))
                         (name (match-string 2))
                         (file (condition-case nil
                                   (find-library-name name)
                                 (expand-file-name
                                  name default-directory))))
                    (when (file-exists-p file)
                      (switch-to-buffer-other-window
                       (find-file-noselect file))
                      (goto-char (or place 1))
                      (when (zerop place)
                        (re-search-forward
                         (format "^(\\(?:cl-\\)?def[-a-z]+ %s"
                                 fn-or-place)
                         nil t)))))))))

;; insert `point-marker' in kill-ring
;;;###autoload
(defun nvp-mark-kill-point ()
  (interactive)
  (kill-new (format "%s" (point-marker))))

;; font-lock markers: #<marker at [point or fn-name] in [filename]>
;;;###autoload
(font-lock-add-keywords
 nil
 ;; 'emacs-lisp-mode
 '(("#<marker at \\([^ \t\n]+\\) in \\([-a-zA-Z0-9.]+\\)>"
    (0 (prog1 ()
         (let* ((place (match-string-no-properties 1))
                (file (match-string-no-properties 2)))
           (put-text-property (1+ (match-beginning 0)) (match-end 0)
                              'display (format "%s<%s>" file place)))))
    (0 'font-lock-constant-face t))))

(provide 'nvp-mark)
;;; nvp-mark.el ends here
