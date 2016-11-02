;;; nvp-indicate --- indicators

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Copyright (C) 2016, Noah Peart, all rights reserved.
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

;;; Temporarily highlight region

;; #<marker at 49961 in ess-utils.el>
;; overlay to blink in current region
(defvar nvp-indicate--region-overlay
  (let ((overlay (make-overlay (point) (point))))
    (overlay-put overlay 'face 'highlight)
    overlay))

;; time to display overlay
(defvar nvp-indicate--blink-delay 0.3)

;;;###autoload
(defun nvp-indicate-blink-region (start end)
  (move-overlay nvp-indicate--region-overlay start end)
  (run-with-timer nvp-indicate--blink-delay nil
                  #'(lambda ()
                      (delete-overlay nvp-indicate--region-overlay))))

;;; Temporarily change modeline color

;; make modeline green for a sec
;;;###autoload
(defun nvp-indicate-modeline-success (&optional msg)
  (let ((color (face-background 'mode-line)))
    (when msg (message msg))
    (set-face-background 'mode-line "#44de11")
    (sit-for 1.5)
    (set-face-background 'mode-line color)
    ;; (add-hook 'post-command-hook #'nvp-indicate-modeline-revert nil t)
    ))

(defun nvp-indicate-modeline-revert (&optional color)
  (remove-hook 'post-command-hook #'nvp-indicate-modeline-revert t)
  (set-face-background 'mode-line color))

(provide 'nvp-indicate)
;;; nvp-indicate.el ends here
