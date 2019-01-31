;;; nvp-info.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-01-31 14:45:28>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Maintainer: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 31 January 2019

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
  (require 'cl-lib)
  (defvar nvp-info-nodes))
(require 'info)

(defun nvp-info-open (topic &optional bname)
  "Open info on TOPIC in BNAME."
  (interactive (list (ido-completing-read "Topic: " nvp-info-nodes)))
  (let ((buff (or bname (concat "*" topic " info*"))))
    (if (get-buffer buff)
        (progn
          (switch-to-buffer buff)
          (unless (string-match topic Info-current-file)
            (Info-goto-node (format "(%s)" topic))))
      (info topic buff))))

;; (when (require 'hydra nil t)
;;   (defhydra nvp-info-hydra-to (:hint nil :color red)
;;     "
;; _m_e _o_rg e_l_isp _e_macs _h_yperspec"
;;     ("m" (nvp-info-open "myshit" "*myshit info*"))
;;     ("c" (nvp-info-open ))
;;     ("o" (nvp-info-open "org" "*org info*"))
;;     ("l" (nvp-info-open "elisp" "*elisp info*"))
;;     ("e" (nvp-info-open "emacs" "*emacs info*"))
;;     ("h" (nvp-info-open "gcl" "*hyperspec*")))

;;   ;; cant remember this shit
;;   (defadvice nvp-info-hydra-to/body (around gimme-help activate)
;;     (let ((hydra-is-helpful t))
;;       ad-do-it)))

(provide 'nvp-info)
;;; nvp-info.el ends here
