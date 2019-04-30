;;; nvp-yaml-indent.el --- better yaml indentation -*- lexical-binding: t; -*-

;;; Commentary:

;; TODO:

;; - Continuation after '|'
;; - appveyor
;; - capf

;;; Code:
(defvar nvp-yaml-indent-offset 2
  "Default indetation offset.")

(defvar nvp-yaml-indent-cont-offset 2
  "Additional offset for continued item entries.")

;; non-nil if current line is block opener
(defun nvp-yaml-indent-block-p ()
  (save-excursion
    (back-to-indentation)
    (unless (looking-at-p "^\\s-*$")
      (skip-chars-forward "^ \t\n\r")
      (eq ?: (char-before (point))))))

;; non-nil if item line starting with '-'. If item then starts with
;; 'if' or 'for' signals :sh, otherwise :item
(defun nvp-yaml-indent-item-p ()
  (save-excursion
    (back-to-indentation)
    (when (eq ?- (char-after (point)))
      (if (progn
            (forward-char)
            (looking-at-p "\\s-*\\(?:if\\|for\\)"))
          :sh
        :item))))

;; continued item
(defun nvp-yaml-indent-item-cont-p ()
  (unless (or (nvp-yaml-indent-block-p)
              (nvp-yaml-indent-item-p))
    (let (done res)
      (save-excursion
        (beginning-of-line)
        (while (and (not done)
                    (not (bobp)))
          (forward-line -1)
          (setq res (nvp-yaml-indent-item-p))
          (setq done (or res (nvp-yaml-indent-block-p)))))
      res)))

(defun nvp-yaml-indent-bash-end-p ()
  (and (eq :sh (nvp-yaml-indent-item-cont-p))
       (save-excursion
         (back-to-indentation)
         (looking-at-p "\\(?:fi\\|done\\)"))))

;; find indentation of previous block ending with a ':', ie
;; |env:
(defun nvp-yaml-indent-previous-block-indent ()
  (save-excursion
    (while (and (not (bobp))
                (not (nvp-yaml-indent-block-p)))
      (forward-line -1))
    (current-indentation)))

(defun nvp-yaml-indent-calculate-indent ()
  (let ((prev (nvp-yaml-indent-previous-block-indent)))
    (cond
     ((or (= 1 (point-at-bol)) (nvp-yaml-indent-block-p))
      prev)
     ((nvp-yaml-indent-item-p)
      (+ prev nvp-yaml-indent-offset))
     (t
      (pcase (nvp-yaml-indent-item-cont-p)
        (:sh (if (nvp-yaml-indent-bash-end-p)
                   (+ (* 2 nvp-yaml-indent-offset) prev)
                 (+ (* 2 nvp-yaml-indent-offset) nvp-yaml-indent-cont-offset prev)))
        (:item (+ nvp-yaml-indent-offset prev))
        (_ (+ nvp-yaml-indent-offset prev)))))))

;; line indentation function, diable interactive when region non-nil
(defun nvp-yaml-indent-line (&optional region dedent)
  (interactive)
  (let ((ci (current-indentation))
        (need (nvp-yaml-indent-calculate-indent)))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (if (and (not region) (or dedent (eq last-command this-command)))
          (if (/= ci 0)
              (indent-to (* (/ (- ci 1) nvp-yaml-indent-offset)
                            nvp-yaml-indent-offset))
            (indent-to (+ nvp-yaml-indent-offset need)))
        (indent-to need)))
    (if (< (current-column) (current-indentation))
        (forward-to-indentation 0))))

(defun nvp-yaml-dedent-line ()
  "De-indent current line."
  (interactive "*")
  (when (and (not (bolp)) (= (current-indentation) (current-column)))
    (nvp-yaml-indent-line nil 'dedent)
    t))

;; indent region, disable interactive
(defun nvp-yaml-indent-region (start end)
  (interactive "r")
  (save-excursion
    (setq end (copy-marker end))
    (goto-char start)
    (let ((pr (unless (minibufferp)
                (make-progress-reporter "Indenting region..." (point) end))))
      (while (< (point) end)
        (or (and (bolp) (eolp))
            (nvp-yaml-indent-line t))
        (forward-line 1)
        (and pr (progress-reporter-update pr (point))))
      (and pr (progress-reporter-done pr))
      (move-marker end nil))))

(provide 'nvp-yaml-indent)
;;; nvp-yaml-indent.el ends here
