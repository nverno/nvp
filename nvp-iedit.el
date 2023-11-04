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

(defvar-local nvp-iedit--idx 0)
(defun nvp-iedit--next ()
  (cl-decf nvp-iedit--idx)
  (and (< nvp-iedit--idx 0)
       (setq nvp-iedit--idx (+ nvp-iedit--idx (length nvp-iedit-restrictions)))))

;; Add iedit bindings
(defvar-keymap iedit-mode-keymap
  :keymap iedit-mode-keymap
  "C-=" #'nvp-iedit-cycle-regions
  "M-o" nil
  "M-O" #'nvp-iedit-occur)

(defvar-keymap nvp-repeat-iedit-cycle-map
  :repeat t
  "=" #'nvp-iedit-cycle-regions)

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
    (setq nvp-iedit--idx (nvp:lsb arg 2))
    (pcase (nth nvp-iedit--idx nvp-iedit-restrictions)
      ('line (iedit-restrict-current-line))
      ('defun (iedit-restrict-function))
      (_ ))
    (nvp:msg "Toggle restrictions with \\[nvp-iedit-cycle-regions]"
      :test (bound-and-true-p iedit-mode) :delay 0.3 :keys t)))

(defun nvp-iedit-report ()
  (let ((message-log-max nil))
    (unless (minibufferp)
      (let ((cur-msg (current-message))
            (msg (format "Restricted to current %s, %d matches."
                         (nth nvp-iedit--idx nvp-iedit-restrictions)
                         (length iedit-occurrences-overlays))))
        (if cur-msg
            (cond ((or (string-prefix-p "Restricted to " cur-msg t)
                       (string-prefix-p "No more matches" cur-msg t))
                   (message msg))
                  ((string-search "[Restricted to " cur-msg)
                   (message "%s [%s]" (replace-regexp-in-string
                                       "\\[Restricted to .*\\'" "" cur-msg)
                            msg))
                  (t (message "%s [%s]" cur-msg msg)))
          (message msg))))))

;; allow expanding of restricted region when in `iedit-mode'
(defun nvp-iedit-cycle-regions ()
  "Cycle `iedit-mode' region restrictions."
  (interactive)
  (when iedit-mode
    (let* ((occ-regexp (iedit-regexp-quote (iedit-current-occurrence-string))))
      (if (region-active-p)
          ;; start 'region cycle from same index as 'line
          (progn (setq nvp-iedit--idx (1- (length nvp-iedit-restrictions)))
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
