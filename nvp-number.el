;;; nvp-number.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'transient)
(nvp:decls)
(nvp:auto "hexl" hexl-hex-string-to-integer hexl-octal-string-to-integer)

;;;###autoload(autoload 'nvp-number-menu "nvp-number")
(transient-define-prefix nvp-number-menu ()
  "Numbers"
  [["Toggle"
    ("t" "Base" nvp-number-toggle-base)
    ("+" "Increment" nvp-number-increment)
    ("-" "Decrement" (lambda ()
                       (interactive)
                       (funcall-interactively #'nvp-number-increment t)))]
   ["Insert"
    ("l" "Limit" nvp-number-insert-limit)
    ("d" "Random decimal" nvp-number-insert-random)
    ("h" "Random hex" nvp-number-insert-random-hex)]
   ["Show"
    ("?" "Decimal" nvp-number-show-decimal)]])

;;;###autoload
(defun nvp-number-increment (arg &optional bnds inc)
  "In/Decrement numbers in region BNDS by INC. 
Decrement with prefix ARG."
  (interactive "P")
  (or bnds (setq bnds (nvp:tap 'bdwim 'symbol))
      (user-error "No region to search in."))
  (nvp:defq inc (if arg -1 1))
  (let (deactivate-mark)
    (nvp-regex-map-across-matches
     (lambda (ms)
       (replace-match (number-to-string (+ inc (string-to-number ms)))))
     "\\([-]?[[:digit:]]+\\)" bnds 1))
  (nvp-repeat-command ?= nil
    `(("+" (lambda nil
             (interactive)
             (nvp-toggle-increment-numbers nil ',bnds))
       :msg "decrement")
      ("-" (lambda nil
             (interactive)
             (nvp-toggle-increment-numbers 1 ',bnds -1))
       :msg "decrement"))))

;; -------------------------------------------------------------------
;;; Conversion

(defvar nvp-number--base '(2 8 10 16))

(defun nvp-number--to-binary (n)
  (let (v)
    (while (> n 0)
      (push (number-to-string (logand n 1)) v)
      (setq n (ash n -1)))
    (mapconcat #'identity v "")))

;;;###autoload
(defun nvp-number-toggle-base (&optional cur-base offset)
  "Toggle base of number at point.
decimal => hex, hex => decimal, octal => decimal."
  (interactive)
  (pcase-let ((`(,beg . ,end) (bounds-of-thing-at-point 'symbol)))
    (unless beg (user-error "Nothing at point"))
    (if (memq ?. (list (char-after end) (char-before beg)))
        (user-error "floating point number")
      (save-match-data
        (let* ((sym (buffer-substring-no-properties beg end))
               (pos (cond
                     ((bound-and-true-p cur-base) (cons offset cur-base))
                     ((string-match "\\`$?0x" sym) (cons (match-end 0) 16))
                     ((string-match "\\`0b" sym) (cons (match-end 0) 2))
                     ((string-match "\\`0o?" sym) (cons (match-end 0) 8))
                     ((string-match "\\`[1-9]" sym)
                      (cons 0 (if (string-match-p "[2-9]" sym) 10 2)))
                     (t nil))))
          (unless pos (user-error "not a number at point"))
          (cl-destructuring-bind (off . from) pos
            (let ((str (substring sym off))
                  (to (nth (mod (1+ (/ from 5)) (length nvp-number--base))
                           nvp-number--base)))
              (unless (or (eq from 16)
                          (string-match-p "[[:digit:]]+$" str))
                (user-error "garbage in number"))
              (setq cur-base to)
              (save-mark-and-excursion
                (delete-region beg end)
                (insert (format
                         (pcase to
                           (2 (setq offset 2)
                              (cl-assert (= from 16))
                              (concat "0b" (nvp-number--to-binary
                                            (string-to-number str from))))
                           (8 (setq offset 0) "%#o")
                           (10 (setq offset 0) "%d")
                           (16 (setq offset 2) "%#x"))
                         (string-to-number str from))))))))))
  (nvp-repeat-command))
   
;;;###autoload
(defun nvp-number-hex-string-to-integer (&optional hex)
  (interactive (list (thing-at-point 'number)))
  (message "%d" (or hex (call-interactively #'hexl-hex-string-to-integer))))

;;;###autoload
(defun nvp-number-octal-string-to-integer ()
  (interactive)
  (message "%d" (call-interactively #'hexl-octal-string-to-integer)))

;; momentary-string-display #<marker at 110240 in subr.el.gz>
;;;###autoload
(defun nvp-number-show-decimal (symbol)
  (interactive (list (thing-at-point 'symbol)))
  (unless symbol (user-error "No symbol at point"))
  (cond
   ((string-match "$?0[xX]" symbol)
    (nvp:with-toggled-tip
     (number-to-string
      (string-to-number (substring symbol (match-end 0)) 16))
     :help-fn nil))))

;; -------------------------------------------------------------------
;;; Insert

(defun nvp-number-insert-limit ()
  (interactive)
  (insert (nvp:read-char-case "Type: " 'verbose
            (?i "[i]i32 max" "2147483647")
            (?I "[I]i32 min" "-2147483648")
            (?l "[l]i64 max" "9223372036854775807")
            (?L "[L]i64 min" "-9223372036854775808"))))

;; https://github.com/skeeto/.emacs.d/blob/master/lisp/extras.el#L83
;;;###autoload
(defun nvp-number-insert-random (n)
  "Insert random number b/w 0 and prefix."
  (interactive "P")
  (insert (number-to-string (random n))))

;;;###autoload
(cl-defun nvp-number-insert-random-hex (&optional (size 64))
  "Insert a random, SIZE-bit number as hex."
  (interactive)
  (let ((string (make-string (/ size 4) 0))
        (digits "0123456789abcdef"))
    (dotimes (i (/ size 4))
      (setf (aref string i) (aref digits (cl-random 16))))
    (insert string)))

(provide 'nvp-number)
;;; nvp-number.el ends here
