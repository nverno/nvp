;;; nvp-iedit.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-01-16 02:08:17>
;; Package-Requires: 
;; Created: 22 August 2018

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
  (require 'cl-lib))
(require 'iedit)

;; Add iedit bindings
(nvp-bind-keys iedit-mode-keymap
  ("C-=" . nvp-iedit-expand))

;; current level of iedit restriction: line, defun, or nil
(defvar-local nvp-iedit-restriction nil)

(advice-add 'iedit-restrict-function
            :after (lambda (&rest _args) (setq nvp-iedit-restriction 'defun)))

(advice-add 'iedit-restrict-current-line
            :after (lambda (&rest _args) (setq nvp-iedit-restriction 'line)))

(add-hook 'iedit-mode-end-hook #'(lambda () (setq nvp-iedit-restriction nil)))
(add-hook 'iedit-aborting-hook #'(lambda () (setq nvp-iedit-restriction nil)))

;;;###autoload
(defun nvp-iedit-dwim (arg)
  "With prefix ARG narrow to defun, with two prefix narrow to current line."
  (interactive "P")
  (if iedit-mode
      (progn
        (setq nvp-iedit-restriction nil)
        (iedit-done))
    (iedit-mode)
    (pcase arg
      (`(16)
       (setq nvp-iedit-restriction 'line)
       (iedit-restrict-current-line))
      (`(4)
       (setq nvp-iedit-restriction 'defun)
       (iedit-restrict-function))
      (_))))

;; allow expanding of restricted region when in `iedit-mode'
(defun nvp-iedit-expand ()
  (interactive)
  (when (and iedit-mode nvp-iedit-restriction)
    (let ((occ-regexp (iedit-current-occurrence-string)))
      (pcase nvp-iedit-restriction
        (`line
         ;; add overlays to matches in function
         (setq nvp-iedit-restriction 'defun)
         (save-mark-and-excursion
           (setq mark-active nil)
           (beginning-of-defun)
           (mark-defun)
           (comment-forward (point-max))
           (let ((beg (region-beginning))
                 (end (region-end)))
             (goto-char beg)
             (while (and (< (point) end)
                         (condition-case nil
                             (iedit-add-occurrence-overlay occ-regexp
                                                           nil 'forward end)
                           (error (forward-word))))))))
        (`defun
            ;; remake overlays in whole buffer
            (setq nvp-iedit-restriction nil)
            (iedit-make-occurrences-overlays
             occ-regexp (point-min) (point-max)))
        (_)))))

(provide 'nvp-iedit)
;;; nvp-iedit.el ends here
