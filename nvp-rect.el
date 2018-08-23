;;; nvp-rect ---  -*- lexical-binding: t; -*-

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
  (require 'hydra)
  (defvar rectangle-mark-mode))
(require 'rect)

;;;###autoload
(defun nvp-rect-load ()
  (interactive)
  (nvp-rect-hydra/body))

;; reset bindings
(global-set-key (kbd "<f2> [") #'nvp-rect-hydra/body)

(defhydra nvp-rect-hydra (:body-pre (rectangle-mark-mode 1)
                                    :color pink
                                    :post (deactivate-mark))
  "
  ^_k_^     _d_elete    _t_ype    _N_umber-lines
_h_   _l_   _y_ank      _o_pen    _e_xchange-point
  ^_j_^     _w_ copy    _r_eset   _r_eset-region-mark
^^^^        _K_ill      _c_lear   _g_ quit
^^^^        _u_ndo      ^ ^        ^ ^
"
  ("h" rectangle-backward-char nil)
  ("l" rectangle-forward-char nil)
  ("k" rectangle-previous-line nil)
  ("j" rectangle-next-line nil)
  ("w" copy-rectangle-as-kill nil)      ;C-x r M-w
  ("K" kill-rectangle)                  ;C-x r k
  ("d" delete-rectangle nil)            ;C-x r d
  ("c" clear-rectangle)                 ;C-x r c
  ("y" yank-rectangle nil)              ;C-x r y
  ("t" string-rectangle nil)            ;C-x r t
  ("o" open-rectangle nil)              ;C-x r o
  ("N" rectangle-number-lines)          ;C-x r N
  ("e" nvp-rect-ex-point-mark nil)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("u" undo nil)
  ("g" nil))
(hydra-set-property 'nvp-rect-hydra :verbosity 1)

;; Exchange point and mark.
(defun nvp-rect-ex-point-mark ()
  (interactive)
  (if rectangle-mark-mode
      (rectangle-exchange-point-and-mark)
    (let ((mk (mark)))
      (rectangle-mark-mode 1)
      (goto-char mk))))

(provide 'nvp-rect)
;;; nvp-rect.el ends here
