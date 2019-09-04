;;; nvp-window.el --- window hydra from git -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'hydra))
(require 'nvp)
(require 'windmove)
(nvp-decl winner-undo winner-redo ace-window ace-swap-window)

(defvar nvp-window--interactive-stack ())
(defvar nvp-window-fast-map
  (let ((map (make-sparse-keymap)))
    (define-key map "," #'nvp-window-configuration-pop)
    (define-key map "d" #'nvp-window-toggle-dedicated)
    (define-key map "s" #'ace-swap-window)
    (define-key map "j" #'ace-window)
    map))

(nvp-advise-commands (apply-partially #'nvp/repeat nvp-window-fast-map)
  :after '(nvp-window-configuration-pop))

;;;###autoload
(defun nvp-window-configuration-push ()
  (interactive)
  (push (current-window-configuration) nvp-window--interactive-stack)
  (message "windows configuration saved"))

;;;###autoload
(defun nvp-window-configuration-pop (&rest _args)
  (interactive)
  (if-let* ((conf (pop nvp-window--interactive-stack)))
      (set-window-configuration conf)
    (message "window configuration stack empty")))

;;;###autoload
(defun nvp-window-toggle-dedicated (window)
  "Toggle WINDOW strongly dedicated."
  (interactive (list (selected-window)))
  (let ((dedicated-p (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated-p))
    (message "window dedicated: %s" (if dedicated-p "off" "on"))))

;; Transpose the buffers shown in two windows.
;; from https://github.com/re5et/.emacs.d/blob/master/my/my-functions.el
;;;###autoload
(defun nvp-window-transpose (arg)
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
	    (next-win (window-buffer (funcall selector))))
	(set-window-buffer (selected-window) next-win)
	(set-window-buffer (funcall selector) this-win)
	(select-window (funcall selector)))
      (setq arg (if (cl-plusp arg) (1- arg) (1+ arg)))))
  (nvp-repeat-command))

;;;###autoload
(defun nvp-window-rotate ()
  (interactive)
  (let ((windows-and-buffers
         (--map (cons it (window-buffer (next-window it))) (window-list))))
    (dolist (window-and-buffer windows-and-buffers)
      (-let (((wnd . buf) window-and-buffer))
        (select-window wnd)
        (switch-to-buffer buf))))
  (nvp-repeat-command))

;;;###autoload
(defun nvp-window-swap ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win (window-buffer))
             (next-win (window-buffer (next-window)))
             (this-edge (window-edges (selected-window)))
             (next-edge (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-edge)
                                         (car next-edge))
                                     (<= (cadr this-edge)
                                         (cadr next-edge)))))
             (splitter
              (if (= (car this-edge)
                     (car (window-edges (next-window))))
                  #'split-window-horizontally
                #'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win)
          (set-window-buffer (next-window) next-win)
          (select-window first-win)
          (if this-win-2nd (other-window 1)))))
  (nvp-repeat-command))

;; Hydra wiki
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
