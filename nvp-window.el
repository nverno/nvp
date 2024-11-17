;;; nvp-window.el --- window maps -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'windmove)
(require 'transient)
(nvp:decls :p (winner ace) :f (ace-window))
(nvp:auto "winner" winner-redo winner-undo)

(defvar nvp-window--interactive-stack ())

(nvp:def-keymap nvp-repeat-window-conf-map
  :repeat (:enter (nvp-window-configuration-pop))
  :wrap (ace-swap-window ace-window)
  "," #'nvp-window-configuration-pop
  "d" #'toggle-window-dedicated
  "s" #'ace-swap-window
  "j" #'ace-window)

(nvp:def-keymap nvp-repeat-window-swap-map
  :repeat t
  :wrap (ace-swap-window)
  "t" #'nvp-window-transpose
  "r" #'nvp-window-rotate
  "|" #'nvp-window-swap
  "s" #'ace-swap-window)
(put 'nvp-repeat-window-swap/ace-swap-window 'repeat-check-key 'no)

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

;; From https://github.com/re5et/.emacs.d/blob/master/my/my-functions.el
;;;###autoload
(defun nvp-window-transpose (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
	    (next-win (window-buffer (funcall selector))))
	(set-window-buffer (selected-window) next-win)
	(set-window-buffer (funcall selector) this-win)
	(select-window (funcall selector)))
      (setq arg (if (cl-plusp arg) (1- arg) (1+ arg))))))

;;;###autoload
(defun nvp-window-rotate ()
  (interactive)
  (let ((windows-and-buffers
         (--map (cons it (window-buffer (next-window it))) (window-list))))
    (dolist (window-and-buffer windows-and-buffers)
      (-let (((wnd . buf) window-and-buffer))
        (select-window wnd)
        (switch-to-buffer buf)))))

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
          (if this-win-2nd (other-window 1))))))


;; -------------------------------------------------------------------
;;; Menu

(defun nvp--menu-split-window-vertical ()
  (interactive)
  (split-window-right)
  (windmove-right))

(defun nvp--menu-split-window-horiz ()
  (interactive)
  (split-window-below)
  (windmove-down))

(defun nvp--menu-ace-swap-windows ()
  (interactive)
  (ace-window 4))

(defun nvp--menu-ace-delete-window ()
  (interactive)
  (ace-window 16))

;; TODO(11/07/24): remove
(defun nvp-window-tst ()
  (interactive)
  (message "frame root: %S" (frame-root-window (selected-window)))
  (with-selected-window (selected-window)
    (call-interactively #'split-root-window-right)))

;;;###autoload(autoload 'nvp-window-menu "nvp-window" nil t)
(transient-define-prefix nvp-window-menu ()
  ;; :refresh-suffixes t
  [["Select"
    ("h" "←" windmove-left :transient t)
    ("j" "↓" windmove-down :transient t)
    ("k" "↑" windmove-up :transient t)
    ("l" "→" windmove-right :transient t)
    " -----"
    ("m" "Swap" nvp--menu-ace-swap-windows :transient t)
    ("M" "Swap states" window-swap-states :transient t)]
   ["Toggle"
    ("i" "Side windows" window-toggle-side-windows :transient t)
    ("D" "Dedicated" toggle-window-dedicated :transient t)]
   ["Split"
    ("v" "Vertical" nvp--menu-split-window-vertical :transient t)
    ("c" "Horizontal" nvp--menu-split-window-horiz :transient t)
    ("sb" "Root below" split-root-window-below :transient t)
    ;; TODO(11/07/24): wrong root from transient menu
    ("sr" "Root right" nvp-window-tst;; split-root-window-right
     :transient t)
    " -----"
    ("u" "Undo" winner-undo :transient t)
    ("y" "Redo" winner-redo :transient t)]
   ["Delete"
    ("dd" "Delete" delete-window :transient t)
    ("da" "Ace delete" nvp--menu-ace-delete-window :transient t)
    ("dv" "Delete vertically" delete-other-windows-vertically :transient t)
    ("do" "Delete all other" delete-other-windows :transient t)]
   ["Resize"
    ("f" "Fit buffer" fit-window-to-buffer :transient t)
    ("S" "Shrink if larger" shrink-window-if-larger-than-buffer :transient t)
    ("b" "Balance" balance-windows :transient t)
    ("B" "Balance areas" balance-windows-area :transient t)
    ("/" "Minimize" minimize-window :transient t)
    ("!" "Maximize" maximize-window :transient t)
    ("r" "Resize" nvp-window-resize-menu :transient transient--do-replace)]]
  ;; TODO(08/20/24): remove
  (interactive)
  (transient-setup 'nvp-window-menu)
  (message "transient window: %S, selected: %S, root on selected: %S"
           (frame-root-window)
           (selected-window)
           (frame-root-window (selected-window))))


;;; Resizing Windows

(defun nvp-window-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let (windmove-wrap-around)
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun nvp-window-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let (windmove-wrap-around)
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun nvp-window-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let (windmove-wrap-around)
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun nvp-window-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let (windmove-wrap-around)
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

;;;###autoload(autoload 'nvp-window-resize-menu "nvp-window" nil t)
(transient-define-prefix nvp-window-resize-menu ()
  ["Resize Window"
   ("h" "←" nvp-window-move-splitter-left :transient t)
   ("j" "↓" nvp-window-move-splitter-down :transient t)
   ("k" "↑" nvp-window-move-splitter-up :transient t)
   ("l" "→" nvp-window-move-splitter-right :transient t)]
  ["Actions"
   ("q" "Back to window menu" nvp-window-menu :transient transient--do-replace)])

(provide 'nvp-window)
;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not docstrings redefine)
;; End:
;;; nvp-window.el ends here
