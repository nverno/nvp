;;; nvp-fold.el --- code folding -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'transient)
(require 'hideshow)
(require 'nvp)
(nvp:decls)

(nvp:transient-define-vars nvp--menu (hs-allow-nesting . t))

(defun nvp-hideshow-menu--quit ()
  (interactive)
  (hs-minor-mode -1))

(transient-define-prefix nvp-hideshow-menu ()
  [["Toggle"
    ("t" "Toggle" hs-toggle-hiding)
    ("q" "Quit" nvp-hideshow-menu--quit)]
   ["Hide"
    ("a" "All" hs-hide-all)
    ("B" "Block" hs-hide-block)
    ("c" "Comments" nvp-hs-hide-comments)
    ("l" "Level" hs-hide-level)]
   ["Show"
    ("s" "All" hs-show-all)
    ("b" "Block" hs-show-block)]
   ["Settings"
    (":n" "Remember nested" nvp--menu-hs-allow-nesting)]]
  (interactive)
  (hs-minor-mode 1)
  (transient-setup 'nvp-hideshow-menu))

(defvar-keymap nvp-repeat-hideshow-toggle-map
  :repeat t
  "<tab>" #'hs-toggle-hiding
  "t"     #'hs-toggle-hiding
  "a"     #'hs-hide-all
  "s"     #'hs-show-all
  "c"     #'nvp-hs-hide-comments
  "l"     #'hs-hide-level
  "f"     #'nvp-hs-toggle)

;; Autoloaded
(nvp:bindings nvp-fold-keymap nil
  :prefix "Fold"
  ("<f2>"  . nvp-hideshow-menu)
  ("<tab>" . hs-toggle-hiding)
  ("a"     . hs-hide-all)
  ("c"     . nvp-hs-hide-comments)
  ("f"     . nvp-hs-toggle)
  ("l"     . hs-hide-level)
  ("s"     . hs-show-all)
  ("q"     . nvp-hs-quit))

;; ignore hidden lines
(defun nvp-move-visual-next5 ()
  (interactive)
  (line-move-visual 5 'noerror)
  (and (fboundp 'do-smooth-scroll)
       (do-smooth-scroll)))

(defun nvp-move-visual-prev5 ()
  (interactive)
  (line-move-visual -5 'noerror)
  (and (fboundp 'do-smooth-scroll)
       (do-smooth-scroll)))

;;; TODO: refactor
(nvp:bindings nvp-hs-fast-map nil
  :with fast-move
  :create t :repeat t :indicate t
  :wrap
  (recenter-top-bottom scroll-up-command scroll-down-command forward-page backward-page)
  ("n"     . nvp-move-visual-next5)
  ("p"     . nvp-move-visual-prev5))

(nvp:bindings hs-minor-mode-map 'hideshow
  ("M-n" . nvp-move-visual-next5)
  ("M-p" . nvp-move-visual-prev5))

(defun nvp-hs-quit ()
  (interactive)
  (hs-minor-mode -1))

;;; Overlay
(defun nvp-hs-display-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'face 'font-lock-comment-face)
    (overlay-put ov 'display
                 (format " ... %d lines"
                         (count-lines (overlay-start ov)
                                      (overlay-end ov))))))

(defvar-local nvp-hs--hidden nil)

;;;###autoload
(defun nvp-hs-toggle (&optional arg)
  "Toggle hideshow on block.  With prefix toggle all."
  (interactive "P")
  (unless (bound-and-true-p hs-minor-mode)
    (hs-minor-mode))
  (if arg ;; (eq last-command 'nvp-hs-toggle)
      (if (setq nvp-hs--hidden (not nvp-hs--hidden))
          (hs-show-all)
        (hs-hide-all))
    (hs-toggle-hiding)))

;; https://www.emacswiki.org/emacs/HideShow
(defun nvp-hs-hide-comments ()
  "Hide all top level comments and display only the first line.
Move point to the beginning of the line, and run the normal hook
`hs-hide-hook'.  See documentation for `run-hooks'."
  (interactive)
  (hs-life-goes-on
   (save-excursion
     ;; (unless hs-allow-nesting
     ;;   (hs-discard-overlays (point-min) (point-max)))
     (goto-char (point-min))
     (let ((spew (make-progress-reporter "Hiding all comment blocks..."
                                         (point-min) (point-max)))
           (re (concat "\\(" hs-c-start-regexp "\\)")))
       (while (re-search-forward re (point-max) t)
         (if (match-beginning 1)
             ;; found a comment, probably
             (let ((c-reg (hs-inside-comment-p)))
               (when (and c-reg (car c-reg))
                 (if (> (count-lines (car c-reg) (nth 1 c-reg)) 1)
                     (hs-hide-block-at-point t c-reg)
                   (goto-char (nth 1 c-reg))))))
         (progress-reporter-update spew (point)))
       (progress-reporter-done spew)))
   (beginning-of-line)
   (run-hooks 'hs-hide-hook)))

;;;###autoload
(defun nvp-hs-mode-hook ()
  (unless (not (eq 'ignore hs-set-up-overlay))
    (setq hs-set-up-overlay 'nvp-hs-display-line-counts)))

;;;###autoload
(add-hook 'hs-minor-mode-hook #'nvp-hs-mode-hook)

(provide 'nvp-fold)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-fold.el ends here
