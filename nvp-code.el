;;; nvp-code.el ---  -*- lexical-binding: t; -*-

;;; Commentary:

;; - code folding
;; - hi-lock
;; - narrow

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'hideshow))
(require 'nvp)
(nvp-decls
 :f (hs-toggle-hiding hs-show-all hs-hide-all hs-discard-overlays
                      hs-inside-comment-p hs-hide-block-at-point)
 :v (hs-minor-mode hs-set-up-overlay hs-allow-nesting hs-c-start-regexp))

(nvp-bindings nvp-fold-keymap nil
  :create t
  :repeat (nvp-hs-hide-comments hs-toggle-hiding)
  :indicate t
  ("TAB" . hs-toggle-hiding)
  ("a"   . hs-hide-all)
  ("c"   . nvp-hs-hide-comments)
  ("f"   . nvp-hs-toggle)
  ("l"   . hs-hide-level)
  ("s"   . hs-show-all))

;;; Hideshow

(defvar-local nvp-hs--hidden nil)

(defun nvp-hs-display-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'face 'font-lock-comment-face)
    (overlay-put ov 'display
                 (format " ... %d lines"
                         (count-lines (overlay-start ov)
                                      (overlay-end ov))))))

(with-eval-after-load "hideshow"
  (unless (not (eq 'ignore hs-set-up-overlay))
    (setq hs-set-up-overlay 'nvp-hs-display-line-counts)))

(defun nvp-hs-mode-hook ()
  (unless hs-set-up-overlay
    (setq hs-set-up-overlay 'nvp-hs-display-line-counts)))

;;;###autoload
(defun nvp-hs-toggle (&optional arg)
  "Toggle hideshow on block.  With prefix toggle all."
  (interactive "P")
  (unless (bound-and-true-p hs-minor-mode)
    (hs-minor-mode))
  (if arg
      (if (setq nvp-hs--hidden (not nvp-hs--hidden))
          (hs-show-all)
        (hs-hide-all))
    (hs-toggle-hiding))
  (nvp-repeat-command nil nil nil 1))

;; https://www.emacswiki.org/emacs/HideShow
(defun nvp-hs-hide-comments ()
  "Hide all top level blocks, if they are comments, displaying only first line.
Move point to the beginning of the line, and run the normal hook
`hs-hide-hook'.  See documentation for `run-hooks'."
  (interactive)
  (hs-life-goes-on
   (save-excursion
     (unless hs-allow-nesting
       (hs-discard-overlays (point-min) (point-max)))
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
(add-hook 'hs-minor-mode-hook #'nvp-hs-mode-hook)

(provide 'nvp-code)
;;; nvp-code.el ends here
