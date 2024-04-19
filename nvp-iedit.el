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

;; Order of restriction cycling, and number of prefixes on initial call to
;; `nvp-iedit-dwim' to start with restriction, eg. C-u => restrict to defun
(defvar nvp-iedit-restrictions '(buffer defun line))
(defconst nvp-iedit--n (length nvp-iedit-restrictions))

(defvar-local nvp-iedit--idx 0)
(defun nvp-iedit--next ()
  "Increment restrictions, returning current restriction before increment."
  (prog1 (nth nvp-iedit--idx nvp-iedit-restrictions)
    (setq nvp-iedit--idx (mod (1+ nvp-iedit--idx) nvp-iedit--n))))

;; Add iedit bindings
(nvp:bindings iedit-mode-keymap :now
  ("M-C-;" . nvp-iedit-cycle-regions)
  ("M-o"   . nil)
  ("M-O"   . nvp-iedit-occur)
  ("C-+"   . iedit-switch-to-mc-mode))

(defvar-keymap nvp-repeat-iedit-cycle-map
  :repeat t
  ";" #'nvp-iedit-cycle-regions)

(defvar-keymap nvp-repeat-iedit-prev-map
  :repeat t
  "SPC" #'iedit-prev-occurrence)
(put 'iedit-prev-occurrence 'repeat-check-key 'no)

;; Push mark before iterating occurrences
(nvp:@push-mark iedit-next-occurrence)
(nvp:@push-mark iedit-prev-occurrence)

;;; Syntax
(defvar-local nvp-iedit-syntax-table nil
  "Special syntax to use with iedit, when non-nil.")

(define-advice iedit-start (:around (orig-fn &rest args) "with-syntax")
  (if (bound-and-true-p nvp-iedit-syntax-table)
      (with-syntax-table nvp-iedit-syntax-table
        (apply orig-fn args))
    (apply orig-fn args)))

(defun nvp-iedit-occur ()
  "Call `occur' with current `iedit-current-occurrence-string'."
  (interactive)
  (occur (regexp-quote (iedit-current-occurrence-string))))

(defun nvp-iedit--apply-restriction (&optional increment)
  "Update matches for current restriction.
If INCREMENT is non-nil, increment restriction before update."
  (let ((prev (and increment (nvp-iedit--next))))
    (pcase (nth nvp-iedit--idx nvp-iedit-restrictions)
      ('line (iedit-restrict-current-line))
      ('defun (cond ((memq prev '(nil buffer))
                     (iedit-restrict-function))
                    ((eq prev 'line)
                     (save-mark-and-excursion
                       (let (mark-active)
                         (nvp-mark-defun)
                         (iedit-make-occurrences-overlays
                          (iedit-regexp-quote
                           (iedit-current-occurrence-string))
                          (region-beginning) (region-end)))))
                    (t nil)))
      (_ (unless (memq prev '(nil buffer))
           (iedit-make-occurrences-overlays
            (iedit-regexp-quote (iedit-current-occurrence-string))
            (point-min) (point-max)))))))

;;;###autoload
(defun nvp-iedit-dwim (&optional arg)
  "With prefix ARG narrow to defun, with two prefix narrow to current line."
  (interactive "p")
  (if iedit-mode (progn (setq nvp-iedit--idx 0)
                        (iedit-done))
    (iedit-mode)
    (setq nvp-iedit--idx (mod (nvp:lsb arg 2) nvp-iedit--n))
    (nvp-iedit--apply-restriction)
    (nvp:msg
      "Toggle restrictions with \\<iedit-mode-keymap>\\[nvp-iedit-cycle-regions]"
      :test (bound-and-true-p iedit-mode)
      :append t)))

(defun nvp-iedit-report (&optional region)
  (nvp:msg-repeated "Restricted to %s, %d matches."
    :clobber "No more matches"
    (if region "region" (nth nvp-iedit--idx nvp-iedit-restrictions))
    (length iedit-occurrences-overlays)))

;; allow expanding of restricted region when in `iedit-mode'
(defun nvp-iedit-cycle-regions ()
  "Cycle `iedit-mode' region restrictions."
  (interactive)
  (when iedit-mode
    (if (region-active-p)
        ;; start 'region cycle from same index as 'line
        (progn (setq nvp-iedit--idx (1- nvp-iedit--n))
               (iedit-restrict-region (region-beginning) (region-end))
               (nvp-iedit-report 'region))
      (nvp-iedit--apply-restriction 'increment)
      (nvp-iedit-report))))

(provide 'nvp-iedit)
;;; nvp-iedit.el ends here
