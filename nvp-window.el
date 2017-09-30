;;; nvp-window --- 

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
  (require 'hydra))
(require 'windmove)
(autoload 'ace-window "ace-window")
(autoload 'winner-undo "winner")
(autoload 'winner-redo "winner")

;;;###autoload
(defun nvp-winmove-init ()
  (interactive)
  (nvp-window/body))

;; bindings
(global-set-key (kbd "<f2> Q") #'nvp-window/body)

(defhydra nvp-window (:color red :hint nil)
   "
Movement^^        ^Split^         ^Switch^      ^Resize^
----------------------------------------------------------------
_j_ ←          _v_ertical      _b_uffer        _r_esize
_k_ ↓          _x_ horizontal  _f_ind files   
_i_ ↑          _z_ undo        _a_ce 1        
_l_ →          _Z_ reset       _s_wap         
^ ^            _D_lt Other     _S_ave         
_q_ quit       _o_nly this     _d_elete    
"
   ("j" windmove-left )
   ("k" windmove-down )
   ("i" windmove-up )
   ("l" windmove-right )
   ("r" nvp-window-resize/body :exit t)
   ("b" helm-mini)
   ("f" helm-find-files)
   ("a" (lambda ()
          (interactive)
          (ace-window 1)
          (add-hook 'ace-window-end-once-hook
                    'nvp-window/body))
    :exit t)
   ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right)))
   ("x" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down)))
   ("s" (lambda ()
          (interactive)
          (ace-window 4)
          (add-hook 'ace-window-end-once-hook
                    'nvp-window/body)))
   ("S" save-buffer)
   ("d" delete-window)
   ("D" (lambda ()
          (interactive)
          (ace-window 16)
          (add-hook 'ace-window-end-once-hook
                    'nvp-window/body)))
   ("o" delete-other-windows)
   ("z" (progn
          (winner-undo)
          (setq this-command 'winner-undo))
   )
   ("Z" winner-redo)
   ("q" nil))
(hydra-set-property 'nvp-window :verbosity 1)

(defhydra nvp-window-resize (:color red)
  "resize"
   ("j" nvp-window-move-splitter-left "←")
   ("k" nvp-window-move-splitter-down "↓")
   ("i" nvp-window-move-splitter-up "↑")
   ("l" nvp-window-move-splitter-right "→")
   ("b" nvp-window/body "back" :exit t)
   ("q" nil "quit"))
(hydra-set-property 'nvp-window-resize :verbosity 1)

;; ------------------------------------------------------------
;;; Functions

;; Move window splitter left.
(defun nvp-window-move-splitter-left (arg)
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

;; Move window splitter right.
(defun nvp-window-move-splitter-right (arg)
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

;; Move window splitter up.
(defun nvp-window-move-splitter-up (arg)
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

;; Move window splitter down.
(defun nvp-window-move-splitter-down (arg)
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

;; -------------------------------------------------------------------
(declare-function helm-mini "helm")
(declare-function helm-find-files "helm")

(provide 'nvp-window)
;;; nvp-window.el ends here
