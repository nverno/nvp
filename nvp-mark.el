;;; nvp-mark.el --- Minor mode for quick xrefs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Minor mode to create xrefs from marker text on the fly
;;
;; TODO:
;; - use overlays
;; - add jump bindings to overlays
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(autoload 'find-library-name "find-func")

(eval-and-compile
  (defvar nvp-mark--regex "#<marker at \\([^ \t\n]+\\) in \\([-a-zA-Z0-9.]+\\)>"))

(defvar-local nvp-mark--fontified-p nil
  "Non-nil if marks in buffer are currently fontified")

(defvar-local nvp-mark--marks () "Position of marks in buffer.")

;; -------------------------------------------------------------------
;;; Util

;; thing-at-point 'marker
(defun nvp-mark-bounds-of-marker-at-point ()
  (save-excursion
    (skip-chars-backward "^#" (line-beginning-position))
    (when (eq ?< (char-after (point)))
      (let ((start (1- (point)))
            (end (progn
                   (skip-chars-forward "^>" (line-end-position))
                   (1+ (point)))))
        (cons start end)))))

(put 'nvp-mark 'bounds-of-thing-at-point 'nvp-mark-bounds-of-marker-at-point)

(defun nvp-mark--collect-marks ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward nvp-mark--regex nil 'move)
      (push (cons (match-beginning 0) (match-end 0)) nvp-mark--marks))))

;; -------------------------------------------------------------------
;; Commands

;;;###autoload
(defun nvp-mark-goto-marker-at-point ()
  "Jump to the marker at point. A marker is of the form
'#<marker at [point/function-name] in [library/relative filename]>'."
  (interactive)
  (when-let* ((bnds (bounds-of-thing-at-point 'nvp-mark)))
    (save-excursion
      (goto-char (car bnds))
      (save-match-data
        (when (re-search-forward nvp-mark--regex (line-end-position) t)
          (let* ((fn-or-place (match-string-no-properties 1))
                 (place (string-to-number fn-or-place))
                 (name (replace-regexp-in-string "\\..*$" "" (match-string 2)))
                 (file (condition-case nil
                           (find-library-name name)
                         (expand-file-name name default-directory))))
            (when (file-exists-p file)
              (switch-to-buffer-other-window (find-file-noselect file))
              (goto-char (or place 1))
              (when (zerop place)
                (re-search-forward
                 (format "^(\\(?:cl-\\)?def[-a-z]+ %s" fn-or-place) nil t)))))))))

;; insert `point-marker' in kill-ring
;;;###autoload
(defun nvp-mark-kill-point ()
  (interactive)
  (kill-new (format "%s" (point-marker))))

;; -------------------------------------------------------------------
;;; Font-lock / Mode

(defun nvp-mark-next (&optional previous)
  "Move to the next nvp-mark.
If PREVIOUS is non-nil, move to the previous nvp-mark."
  (interactive)
  (let ((curr (point)))
    (beginning-of-line)
    (skip-syntax-forward " <")
    (and (looking-at-p nvp-mark--regex)
         (ignore-errors (forward-line (if previous -1 1))))
    (condition-case nil  ;if no match return point
        (progn
          (funcall
           (if previous 're-search-backward 're-search-forward) nvp-mark--regex)
          (goto-char (match-beginning 0)))
      (error (progn (message
                     (format "No %s marks" (if previous "previous" "next")))
                    (goto-char curr))))))

;; #<marker at 3312 in nvp-mark.el>
(defun nvp-mark-previous ()
  "Move to the previous nvp-mark."
  (interactive)
  (nvp-mark-next 'previous))

;; Font-lock
(defvar nvp-mark-font-lock-keywords
  `((,nvp-mark--regex
     (0 (prog1 ()
          (let* ((place (match-string-no-properties 1))
                 (file (match-string-no-properties 2)))
            (put-text-property (1+ (match-beginning 0)) (match-end 0)
                               'display (format "%s<%s>" file place)))))
     (0 'font-lock-constant-face t))))

;; remove special mark display when disabling fontification
(defun nvp-mark--remove-display ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward nvp-mark--regex nil 'move)
      (remove-text-properties (match-beginning 0) (match-end 0) '(display)))))

;;;###autoload
(defun nvp-mark-toggle-fontification ()
  "Toggle fontification of nvp-marks in buffer."
  (interactive)
  (if (setq nvp-mark--fontified-p (not nvp-mark--fontified-p))
      (progn
        (nvp-mark--remove-display)
        (font-lock-refresh-defaults))
    (font-lock-add-keywords nil nvp-mark-font-lock-keywords)
    (font-lock-flush)
    (font-lock-ensure)))

(defvar nvp-mark-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "M-s-p") #'nvp-mark-previous)
    (define-key km (kbd "M-s-n") #'nvp-mark-next)
    (define-key km (kbd "M-s-<return>") #'nvp-mark-goto-marker-at-point)
    (define-key km [mouse-1] #'nvp-mark-goto-marker-at-point)
    (easy-menu-define nil km nil
      '("NMark"
        ["Previous" nvp-mark-previous t]
        ["Next" nvp-mark-next t]))
    km)
  "Keymap used in `nvp-mark-mode'.")

;;;###autoload
(define-minor-mode nvp-mark-mode "NvpMark"
  :lighter " NMark"
  :keymap nvp-mark-mode-map
  (if nvp-mark-mode
      (font-lock-add-keywords nil nvp-mark-font-lock-keywords)
    (nvp-mark--remove-display)
    (font-lock-remove-keywords nil nvp-mark-font-lock-keywords))
  (font-lock-flush)
  (font-lock-ensure))

(provide 'nvp-mark)
;;; nvp-mark.el ends here
