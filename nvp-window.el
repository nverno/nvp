;;; nvp-window.el --- window hydra from git -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'hydra))
(require 'windmove)
(declare-function ace-window "ace-window")
(nvp-autoload :pkg "winner" winner-undo winner-redo)

;;;###autoload
(defun nvp-window-toggle-dedicated (window)
  "Toggle WINDOW strongly dedicated."
  (interactive (list (selected-window)))
  (let ((dedicated-p (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated-p))))

;;;###autoload(autoload 'nvp-window-hydra/body "nvp-window")
(nvp-hydra-set-property 'nvp-window-hydra)
(defhydra nvp-window-hydra (:color red)
   "
Movement^^        ^Split^       ^Switch^   ^Resize^
----------------------------------------------------------------
_j_ ←          _v_ertical        _a_ce 1   _r_esize
_k_ ↓          _x_ horizontal    _s_wap    _D_lt Other
_i_ ↑          _z_ undo          _S_ave    _o_nly this  
_l_ →          _Z_ reset         _d_elete  _q_uit
"                                     
   ("j" windmove-left)
   ("k" windmove-down)
   ("i" windmove-up)
   ("l" windmove-right)
   ("r" nvp-window-resize-hydra/body :exit t)
   ("a" (lambda ()
          (interactive)
          (ace-window 1)
          (add-hook 'ace-window-end-once-hook
                    'nvp-window-hydra/body))
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
                    'nvp-window-hydra/body)))
   ("S" save-buffer)
   ("d" delete-window)
   ("D" (lambda ()
          (interactive)
          (ace-window 16)
          (add-hook 'ace-window-end-once-hook
                    'nvp-window-hydra/body)))
   ("o" delete-other-windows)
   ("z" (progn
          (winner-undo)
          (setq this-command 'winner-undo)))
   ("Z" winner-redo)
   ("q" nil))

;; -------------------------------------------------------------------
;;; Resizing windows

(nvp-hydra-set-property 'nvp-window-resize-hydra)
(defhydra nvp-window-resize-hydra (:color red)
  "resize"
   ("j" nvp-window-move-splitter-left "←")
   ("k" nvp-window-move-splitter-down "↓")
   ("i" nvp-window-move-splitter-up "↑")
   ("l" nvp-window-move-splitter-right "→")
   ("b" nvp-window-hydra/body "back" :exit t)
   ("q" nil "quit"))

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

(provide 'nvp-window)
;;; nvp-window.el ends here
