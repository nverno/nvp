;;; nvp-util.el --- Various utility functinos -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; - strings: some modified s.el that aren't covered in subr-x, various others
;; - lists: flatten, intersection, split
;; - regexps
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)

;; -------------------------------------------------------------------
;;; Strings 

;; (defun nvp-s-random-words (num &optional max-len)
;;   "Make a string of NUM random 'words' of MAX-LEN (default 8)."
;;   (declare (side-effect-free t))
;;   (or max-len (setq max-len 8))
;;   (let (ss res (alpha "abcdefghijklmnopqrstuvwxyz"))
;;     (dotimes (_ num)
;;       (setq ss (make-string (1+ (random max-len)) 0))
;;       (dotimes (i (length ss))
;;         (setf (aref ss i) (aref alpha (random 26))))
;;       (setq res (cons ss res)))
;;     (mapconcat 'identity res " ")))

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

;; -------------------------------------------------------------------
;;; Regexps

(eval-when-compile
  (defmacro nvp-regex:with-matches (regex bounds subexp &rest body)
    (declare (indent 3))
    (nvp:with-syms (beg end)
      `(-let (((,beg . ,end) ,bounds))
         (save-excursion
           (if ,beg (goto-char ,beg)
             (setq ,beg (point)))
           (save-match-data
             (while (and (< ,beg ,end)
                         (re-search-forward ,regex ,end 'move))
               (setq ,beg (1+ (match-beginning ,subexp)))
               ,@body)))))))

(defun nvp-regex-map-across-matches (fun regex bnds subexp)
  "Apply FUN to all REGEX matches in BNDS.
If non-nil use SUBEXP regexp group."
  (nvp-regex:with-matches regex bnds subexp
    (funcall fun (match-string subexp))))

;; (defun nvp-regex-all-matches (regex bnds subexp)
;;   "Find all matches of REGEX w/in region BNDS.
;; Regex matches are collected for SUBEXP (default 0)."
;;   (let (matches)
;;     (nvp-regex:with-matches regex bnds subexp
;;       (push (match-string-no-properties subexp) matches))
;;     (nreverse matches)))

;; (defun nvp-regex-all-match-positions (regex bnds subexp)
;;   "Same parameters as `nvp-regex-all-matches', but gathers match positions."
;;   (let (positions)
;;     (nvp-regex:with-matches regex bnds subexp
;;       (push (cons (match-beginning subexp) (match-end subexp)) positions))
;;     (nreverse positions)))

(provide 'nvp-util)
;;; nvp-util.el ends here
