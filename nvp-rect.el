;;; nvp-rect --- 

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
  ^_i_^     _d_elete    _s_tring
_j_   _l_   _o_k        _y_ank
  ^_k_^     _c_opy      _r_eset
^^^^        _e_xchange  _u_ndo
^^^^        ^ ^         _p_aste
"
  ("j" rectangle-backward-char nil)
  ("l" rectangle-forward-char nil)
  ("i" rectangle-previous-line nil)
  ("k" rectangle-next-line nil)
  ("e" nvp-rect-ex-point-mark nil)
  ("c" copy-rectangle-as-kill nil)
  ("d" delete-rectangle nil)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("y" yank-rectangle nil)
  ("u" undo nil)
  ("s" string-rectangle nil)
  ("p" kill-rectangle nil)
  ("o" nil nil))

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
