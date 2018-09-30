;;; nvp-mark ---  -*- lexical-binding: t; -*-

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

(defvar nvp-mark--regex "#<marker at \\([^ \t\n]+\\) in \\([-a-zA-Z0-9.]+\\)>")

(defvar-local nvp-mark--fontified-p nil
  "Non-nil if marks in buffer are currently fontified")

(defvar-local nvp-mark--marks () "Position of marks in buffer.")

;; -------------------------------------------------------------------
;;; Util

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

(put 'marker 'bounds-of-thing-at-point 'nvp-mark-bounds-of-marker-at-point)

(defun nvp-mark--collect-marks ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward nvp-mark--regex nil 'move)
      (push (cons (match-beginning 0) (match-end 0)) nvp-mark--marks))))

;; -------------------------------------------------------------------
;; Commands

;;;###autoload
(defun nvp-mark-goto-marker-at-point ()
  "Jump to the marker at point.
A marker is of the form \
'#<marker at [point/function-name] in [library/relative filename]>'."
  (interactive)
  (when-let* ((bnds (bounds-of-thing-at-point 'marker)))
    (save-excursion
      (goto-char (car bnds))
      (save-match-data
        (when (re-search-forward nvp-mark--regex (line-end-position) t)
          (let* ((fn-or-place (match-string-no-properties 1))
                 (place (string-to-number fn-or-place))
                 (name (match-string 2))
                 (file (condition-case nil
                           (find-library-name name)
                         (expand-file-name name default-directory))))
            (when (file-exists-p file)
              (switch-to-buffer-other-window (find-file-noselect file))
              (goto-char (or place 1))
              (when (zerop place)
                (re-search-forward
                 (format "^(\\(?:cl-\\)?def[-a-z]+ %s" fn-or-place) nil t)))))))))

;; insert `point-marker' in kill-ring
;;;###autoload
(defun nvp-mark-kill-point ()
  (interactive)
  (kill-new (format "%s" (point-marker))))

;; -------------------------------------------------------------------
;;; Font-lock / Mode

(defun nvp-mark-next (&optional previous)
  "Move to the next nvp-mark.
If PREVIOUS is non-nil, move to the previous nvp-mark."
  (interactive)
  (let ((curr (point)))
    (beginning-of-line)
    (skip-syntax-forward " <")
    (and (looking-at-p nvp-mark--regex)
         (ignore-errors (forward-line (if previous -1 1))))
    (condition-case nil  ;if no match return point
        (progn
          (funcall
           (if previous 're-search-backward 're-search-forward) nvp-mark--regex)
          (goto-char (match-beginning 0)))
      (error (progn (message
                     (format "No %s marks" (if previous "previous" "next")))
                    (goto-char curr))))))

;; #<marker at 3312 in nvp-mark.el>
(defun nvp-mark-previous ()
  "Move to the previous nvp-mark."
  (interactive)
  (nvp-mark-next 'previous))

(eval-when-compile
  ;; Do mark fontification
  (defmacro nvp-mark--apply-font-lock ()
    `'((,nvp-mark--regex
        (0 (prog1 ()
             (let* ((place (match-string-no-properties 1))
                    (file (match-string-no-properties 2)))
               (put-text-property (1+ (match-beginning 0)) (match-end 0)
                                  'display (format "%s<%s>" file place)))))
        (0 'font-lock-constant-face t)))))

;; remove special mark display when disabling fontification
(defun nvp-mark--remove-display ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward nvp-mark--regex nil 'move)
      (set-text-properties (match-beginning 0) (match-end 0) nil))))

;;;###autoload
(defun nvp-mark-toggle-fontification ()
  "Toggle fontification of nvp-marks in buffer."
  (interactive)
  (if (setq nvp-mark--fontified-p (not nvp-mark--fontified-p))
      (progn
        (nvp-mark--remove-display)
        (font-lock-refresh-defaults))
    (font-lock-add-keywords nil (nvp-mark--apply-font-lock))
    (font-lock-flush)
    (font-lock-ensure)))

(defvar nvp-mark-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "M-s-p") #'nvp-mark-previous)
    (define-key km (kbd "M-s-n") #'nvp-mark-next)
    (easy-menu-define nil km nil
      '("NMark"
        ["Previous" nvp-mark-previous t]
        ["Next" nvp-mark-next t]))
    km)
  "Keymap used in `nvp-mark-mode'.")

;;;###autoload
(define-minor-mode nvp-mark-mode "NvpMark"
  nil
  :lighter " NMark"
  :keymap nvp-mark-mode-map
  (if nvp-mark-mode
      (font-lock-add-keywords nil (nvp-mark--apply-font-lock))
    (nvp-mark--remove-display)
    (font-lock-remove-keywords nil (nvp-mark--apply-font-lock)))
  (font-lock-flush)
  (font-lock-ensure))

;; -------------------------------------------------------------------
;;; Advices

;;;###autoload
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

(provide 'nvp-mark)
;;; nvp-mark.el ends here
