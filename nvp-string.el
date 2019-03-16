;;; nvp-string.el --- string manipulations -*- lexical-binding: t; -*-

;; Last modified: <2019-03-16 04:39:37>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 21 February 2019

;;; Commentary:
;; some functions from s.el that aren't covered in subr-x, various others
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'nvp-macro))

(defun nvp-s-random-words (num &optional max-len)
  "Make a string of NUM random 'words' of MAX-LEN (default 8)."
  (declare (side-effect-free t))
  (or max-len (setq max-len 8))
  (let (ss res (alpha "abcdefghijklmnopqrstuvwxyz"))
    (dotimes (_ num)
      (setq ss (make-string (1+ (random max-len)) 0))
      (dotimes (i (length ss))
        (setf (aref ss i) (aref alpha (random 26))))
      (setq res (cons ss res)))
    (mapconcat 'identity res " ")))

(defun nvp-s-wrap (len s &optional prefix)
  "If S is longer than LEN, wrap and optionally append PREFIX to each line.
Like `s-word-wrap' but allow for PREFIX."
  (declare (side-effect-free t))
  (save-match-data
    (with-temp-buffer
      (when prefix
        (insert prefix)
        (set-fill-prefix))
      (insert s)
      (let ((fill-column len))
        (fill-region (point-min) (point-max)))
      (buffer-string))))

(defun nvp-s-all-matches (regex str &optional group)
  "Find all matches of REGEX in STR for regex GROUP (default 0)."
  (declare (side-effect-free t))
  (or group (setq group 0))
  (save-match-data
    (let ((pos 0) (len (length str)) matches)
      (while (and (< pos len)
                  (string-match regex str pos))
        (setq pos (1+ (match-beginning group)))
        (push (match-string group str) matches))
      (nreverse matches))))

(defun nvp-s-all-match-positions (regex str &optional group)
  "Find positions of all REGEX matches in STR for regex GROUP (default 0)."
  (declare (side-effect-free t))
  (or group (setq group 0))
  (save-match-data
    (let ((pos 0) (len (length str)) positions)
      (while (and (< pos len) (string-match regex str pos))
        (setq pos (match-end group))
        (push (cons (match-beginning group) (match-end group)) positions))
      (nreverse positions))))

(defun nvp-s-center (len s &optional char)
  "If S is shorter than LEN, pad it with CHAR (default spaces) so it's centered.
Like `s-center' but allow for CHAR."
  (declare (pure t) (side-effect-free t))
  (or char (setq char ? ))
  (let ((extra (max 0 (- len (length s)))))
    (concat (make-string (ceiling extra 2) char)
            s
            (make-string (floor extra 2) char))))

(defun nvp-s-repeat (num s)
  "Make a string of S repeated NUM times."
  (declare (pure t) (side-effect-free t))
  (let (ss)
    (dotimes (_ num)
      (setq ss (cons s ss)))
    (apply #'concat ss)))

(provide 'nvp-string)
;;; nvp-string.el ends here
