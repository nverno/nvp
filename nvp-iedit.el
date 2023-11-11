;;; nvp-iedit.el --- mark/replace matches -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; iedit extensions:
;; - toggle match restriction b/w global, defun, line
;; - expand regions during iedit
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'iedit)
(nvp:decls)

(defvar nvp-iedit-restrictions '(buffer defun line))
(defconst nvp-iedit--n (length nvp-iedit-restrictions))

(defvar-local nvp-iedit--idx 0)
(defun nvp-iedit--next ()
  (setq nvp-iedit--idx
        (mod (+ nvp-iedit--idx (1- nvp-iedit--n)) nvp-iedit--n)))

;; Add iedit bindings
(nvp:bindings iedit-mode-keymap :now
  ("M-C-;" . nvp-iedit-cycle-regions)
  ("M-o"   . nil)
  ("M-O"   . nvp-iedit-occur))

(defvar-keymap nvp-repeat-iedit-cycle-map
  :repeat t
  ";" #'nvp-iedit-cycle-regions)

(defvar-keymap nvp-repeat-iedit-prev-map
  :repeat t
  "SPC" #'iedit-prev-occurrence)
(put 'iedit-prev-occurrence 'repeat-check-key 'no)


(defun nvp-iedit-occur ()
  "Call `occur' with current `iedit-current-occurrence-string'."
  (interactive)
  (occur (regexp-quote (iedit-current-occurrence-string))))

;;;###autoload
(defun nvp-iedit-dwim (&optional arg)
  "With prefix ARG narrow to defun, with two prefix narrow to current line."
  (interactive "p")
  (if iedit-mode (progn (setq nvp-iedit--idx 0)
                        (iedit-done))
    (iedit-mode)
    (setq nvp-iedit--idx (mod (nvp:lsb arg 2) nvp-iedit--n))
    (pcase (nth nvp-iedit--idx nvp-iedit-restrictions)
      ('line (iedit-restrict-current-line))
      ('defun (iedit-restrict-function))
      (_ ))
    (nvp:msg "Toggle restrictions with \\[nvp-iedit-cycle-regions]"
      :test (bound-and-true-p iedit-mode)
      :keys t :keymap iedit-mode-keymap
      :append t)))

(defun nvp-iedit-report ()
  (nvp:msg-repeated "Restricted to %s, %d matches."
    :clobber "No more matches"
    (nth nvp-iedit--idx nvp-iedit-restrictions)
    (length iedit-occurrences-overlays)))

;; allow expanding of restricted region when in `iedit-mode'
(defun nvp-iedit-cycle-regions ()
  "Cycle `iedit-mode' region restrictions."
  (interactive)
  (when iedit-mode
    (let* ((occ-regexp (iedit-regexp-quote (iedit-current-occurrence-string))))
      (if (region-active-p)
          ;; start 'region cycle from same index as 'line
          (progn (setq nvp-iedit--idx (1- nvp-iedit--n))
                 (iedit-restrict-region (region-beginning) (region-end)))
        (nvp-iedit--next)
        (pcase (nth nvp-iedit--idx nvp-iedit-restrictions)
          ('defun (save-mark-and-excursion
                    (setq mark-active nil)
                    (nvp-mark-defun)
                    (let ((beg (region-beginning))
                          (end (region-end)))
                      (goto-char beg)
                      (while (and (< (point) end)
                                  (condition-case nil
                                      (iedit-add-occurrence-overlay
                                       occ-regexp nil 'forward end)
                                    (error (forward-word))))))))
          ('buffer (iedit-make-occurrences-overlays
                    occ-regexp (point-min) (point-max)))
          (_ (iedit-restrict-current-line)))
        (nvp-iedit-report)))))

(provide 'nvp-iedit)
;;; nvp-iedit.el ends here
