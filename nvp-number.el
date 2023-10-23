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
(defun nvp-number-hex-string-to-integer (&optional hex)
  (interactive (list (thing-at-point 'number)))
  (message "%d" (or hex (call-interactively #'hexl-hex-string-to-integer))))

;;;###autoload
(defun nvp-number-octal-string-to-integer ()
  (interactive)
  (message "%d" (call-interactively #'hexl-octal-string-to-integer)))

(defun nvp-number--to-binary (n)
  (let (v)
    (while (> n 0)
      (push (number-to-string (logand n 1)) v)
      (setq n (ash n -1)))
    (mapconcat #'identity v "")))

(defvar nvp-number--base '(2 8 10 16))

;;;###autoload
(defun nvp-number-toggle-base (&optional cur-base offset)
  "Toggle base of number at point.
decimal => hex, hex => decimal, octal => decimal."
  (interactive)
  (pcase-let ((`(,beg . ,end) (bounds-of-thing-at-point 'symbol)))
    (if (memq ?. (list (char-after end) (char-before beg)))
        (user-error "floating point number")
      (save-match-data
        (let* ((sym (buffer-substring-no-properties beg end))
               (pos (cond
                      (cur-base (cons offset cur-base))
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
  ;; FIXME: args aren't lexically captured if called like
  ;; (nvp-repeat-command nil nil nil cur-base offset)
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
