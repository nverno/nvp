;;; nvp-number.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls)
(nvp:auto "hexl" 'hexl-octal-string-to-integer 'hexl-hex-string-to-integer)

;; -------------------------------------------------------------------
;;; Conversion

;; from hexl.el
;;;###autoload
(defun nvp-number-hex-string-to-integer ()
  (interactive)
  (message "%d" (call-interactively #'hexl-hex-string-to-integer)))

;;;###autoload
(defun nvp-number-octal-string-to-integer ()
  (interactive)
  (message "%d" (call-interactively #'hexl-octal-string-to-integer)))

;;;###autoload
(defun nvp-number-toggle-base ()
  "Toggle base of number at point.
decimal => hex, hex => decimal, octal => decimal."
  (interactive)
  (pcase-let ((`(,beg . ,end) (bounds-of-thing-at-point 'symbol)))
    (if (memq ?. (list (char-after end) (char-before beg)))
        (user-error "floating point number")
      (save-match-data
        (let* ((sym (buffer-substring-no-properties beg end))
               (base (cond
                      ((string-match "\\`$?0x" sym) (list (match-end 0) 16 10))
                      ((string-match "\\`0o?" sym) (list (match-end 0) 8 10))
                      ((string-match "\\`[1-9]" sym) (list 0 10 16))
                      (t nil))))
          (unless base (user-error "not a number at point"))
          (cl-destructuring-bind (pos from to) base
            (let ((str (substring sym pos)))
              (unless (or (eq from 16)
                          (string-match-p "[[:digit:]]+$" str))
                (user-error "garbage in number"))
              (save-mark-and-excursion
                (delete-region beg end)
                (insert (format (pcase to
                                  (10 "%d")
                                  (16 "0x%x")
                                  (8 "0%o"))
                                (string-to-number str from))))))))))
  (nvp-repeat-command))

;; -------------------------------------------------------------------
;;; Popups
;; momentary-string-display #<marker at 110240 in subr.el.gz>

;;;###autoload
(defun nvp-number-show-decimal (symbol)
  (interactive (list (thing-at-point 'symbol)))
  (cond
   ((string-match "$?0x" symbol)
    (nvp:with-toggled-tip
     (number-to-string
      (string-to-number (substring symbol (match-end 0)) 16))
     :help-fn nil))))

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
