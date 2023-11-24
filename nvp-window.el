;;; nvp-window.el --- window maps -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'windmove)
(require 'transient)
(nvp:decls :p (winner ace))
(nvp:auto "winner" winner-redo winner-undo)

(defvar nvp-window--interactive-stack ())

(nvp:def-keymap nvp-repeat-window-conf-map
  :wrap (ace-swap-window ace-window)
  :repeat (:enter (nvp-window-configuration-pop))
  "," #'nvp-window-configuration-pop
  "d" #'nvp-window-toggle-dedicated
  "s" #'nvp-repeat-window-conf/ace-swap-window
  "j" #'nvp-repeat-window-conf/ace-window)

(nvp:def-keymap nvp-repeat-window-swap-map
  :wrap (ace-swap-window)
  :repeat t
  "t" #'nvp-window-transpose
  "r" #'nvp-window-rotate
  "|" #'nvp-window-swap
  "s" #'nvp-repeat-window-swap/ace-swap-window)
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
;;; Transient menu

;;;###autoload(autoload 'nvp-window-menu "nvp-window")
(transient-define-prefix nvp-window-menu ()
  [["Movement"
    ("j" "←" windmove-left :transient t)
    ("k" "↓" windmove-down :transient t)
    ("i" "↑" windmove-up :transient t)
    ("l" "→" windmove-right :transient t)]
   ["Split"
    ("v" "Vertical" (lambda ()
                      (interactive)
                      (split-window-right)
                      (windmove-right))
     :transient t)
    ("x" "Horizontal" (lambda ()
                        (interactive)
                        (split-window-below)
                        (windmove-down))
     :transient t)
    ("z" "Undo" (lambda ()
                  (interactive)
                  (winner-undo)
                  (setq this-command 'winner-undo))
     :transient t)
    ("Z" "Reset" winner-redo :transient t)]
   ["Switch"
    ("a" "Ace" (lambda ()
                 (interactive)
                 (ace-window 1)))
    ("s" "Swap" (lambda ()
                  (interactive)
                  (ace-window 4))
     :transient t)]
   ["Delete"
    ("d" "Delete" delete-window :transient t)
    ("D" "Ace delete" (lambda ()
                        (interactive)
                        (ace-window 16))
     :transient t)
    ("o" "Delete all other" delete-other-windows :transient t)]
   ["Resize"
    ("r" "Resize" nvp-window-resize-menu :transient transient--do-replace)]])

;; -------------------------------------------------------------------
;;; Resizing windows

(transient-define-prefix nvp-window-resize-menu ()
  ["Resize"
   ("j" "←" nvp-window-move-splitter-left :transient t)
   ("k" "↓" nvp-window-move-splitter-down :transient t)
   ("i" "↑" nvp-window-move-splitter-up :transient t)
   ("l" "→" nvp-window-move-splitter-right :transient t)
   ("b" "Back" nvp-window-menu :transient transient--do-replace)])

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
;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not docstrings redefine)
;; End:
;;; nvp-window.el ends here
