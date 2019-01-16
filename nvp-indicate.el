;;; nvp-indicate.el --- indicators -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-01-16 02:05:02>
;; Package-Requires: 
;; Created:  2 November 2016

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

;; time to display overlay
(defvar nvp-indicate--blink-delay 0.3)

;; store indicators
(defvar nvp-indicate--cache (make-hash-table))

;;;###autoload
(defun nvp-indicate-cache (key &optional value overwrite)
  (if value
      (if overwrite (puthash key value nvp-indicate--cache)
        (or (gethash key nvp-indicate--cache)
            (puthash key value nvp-indicate--cache)))
    (gethash key nvp-indicate--cache)))

;;; Temporarily highlight region

;; #<marker at 49961 in ess-utils.el>
;; overlay to blink in current region
(defvar nvp-indicate--region-overlay
  (let ((overlay (make-overlay (point) (point))))
    (overlay-put overlay 'face 'highlight)
    overlay))

;;;###autoload
(defun nvp-indicate-blink-region (start end)
  (move-overlay nvp-indicate--region-overlay start end)
  (run-with-timer nvp-indicate--blink-delay nil
                  #'(lambda ()
                      (delete-overlay nvp-indicate--region-overlay))))

;;; Hydra pre/post change cursor color
(defun nvp-indicate-hydra-pre ()
  (nvp-indicate-cache 'cursor-color (face-attribute 'cursor :background))
  (set-cursor-color "#e31422"))

(defun nvp-indicate-hydra-post ()
  (set-cursor-color (nvp-indicate-cache 'cursor-color)))

;; ------------------------------------------------------------
;;; Temporarily change modeline color

;; make modeline green for a sec
;;;###autoload
(defun nvp-indicate-modeline-success (&optional msg)
  (let* ((power-p (memq 'powerline-active0 (face-list)))
         (pcolor (and power-p (face-background 'powerline-active0)))
         (color (face-background 'mode-line)))
    (when msg (message (concat "[SUCCESS]" msg)))
    (set-face-background 'mode-line "#44de11")
    (and power-p (set-face-background 'powerline-active0 "#44de11"))
    (sit-for 1.5)
    (set-face-background 'mode-line color)
    (and power-p (set-face-background 'powerline-active0 pcolor))
    ;; (add-hook 'post-command-hook #'nvp-indicate-modeline-revert nil t)
    ))

(defun nvp-indicate-modeline-revert (&optional color)
  (remove-hook 'post-command-hook #'nvp-indicate-modeline-revert t)
  (set-face-background 'mode-line color))

;; ------------------------------------------------------------
;;; Toggle font-locking for long lines

(defvar-local nvp-indicate--add-font t)
;;;###autoload
(defun nvp-indicate-long-lines (arg)
  (interactive "P")
  (if (setq nvp-indicate--add-font (not nvp-indicate--add-font))
      (font-lock-refresh-defaults)
    (let ((len (if arg (read-number "Length: ") 80)))
      (font-lock-add-keywords
       nil `((,(format "^[^\n]\\{%d\\}\\(.*\\)$" len)
              1 font-lock-warning-face t)))
      (font-lock-flush)
      (font-lock-ensure))))

(provide 'nvp-indicate)
;;; nvp-indicate.el ends here
