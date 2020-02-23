;;; nvp-util.el --- Various utility functinos -*- lexical-binding: t; -*-

;;; Commentary:
;; - strings: some modified s.el that aren't covered in subr-x, various others
;; - lists: flatten, intersection, split
;; - regexps
;; - save vars
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)

;;; Files

(defun nvp-file-locate-first-dominating (file names)
  "Locate first name in NAMES using `locate-dominating-file' starting from FILE."
  (cl-loop for name in names
     as res = (locate-dominating-file file name)
     when res
     return res))

(defun nvp-file-subdirs (dir)
  "Retutn alist of (dir-name . full-path) for subdirectories of DIR."
  (delq nil
        (--map
         (let (y)
           (when (file-directory-p (setq y (expand-file-name it dir)))
             (cons it y)))
         (cl-set-difference (directory-files dir) '("." "..") :test #'equal))))


;;; Strings

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


;;; Lists

;; if ELEM is a list and has a null cdr return its car, otherwise return it
(defsubst nvp-flatten--elem (elem)
  (if (and (consp elem) (null (cdr elem))) (car elem) elem))

(defun nvp-flatten-to-alist (tree)
  "Flatten tree, but leave cons cells. 
The result may also contain atoms that where head of subalists."
  (declare (pure t) (side-effect-free t))
  (let (elems)
    (while (and (consp tree))
      (let ((elem (pop tree)))
        (while (and (consp elem) (consp (cdr elem)))
          (push (cdr elem) tree)
          (setq elem (car elem)))
        (if elem (push (nvp-flatten--elem elem) elems))))
    (if tree (push (nvp-flatten--elem tree) elems))
    (nreverse elems)))

(defun nvp-flatten-tree (lst &optional alist)
  "Flatten nested list. If ALIST is non-nil, leave cons cells intact."
  (declare (pure t) (side-effect-free t))
  (if alist (nvp-flatten-to-alist lst)
    (flatten-tree lst)))

;; Intersection of multiple lists.
(defun nvp-list-intersection (l)
  (declare (pure t) (side-effect-free t))
  (cond ((null l) nil)
	((null (cdr l)) (car l))
	(t (cl-intersection (car l) (nvp-list-intersection (cdr l))))))

;; -------------------------------------------------------------------
;;; Regexps

(eval-when-compile
  (defmacro nvp-regex:with-matches (regex bounds subexp &rest body)
    (declare (indent 3))
    (nvp-with-syms (beg end)
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

(defun nvp-regex-all-matches (regex bnds subexp)
  "Find all matches of REGEX w/in region BNDS.
Regex matches are collected for SUBEXP (default 0)."
  (let (matches)
    (nvp-regex:with-matches regex bnds subexp
      (push (match-string-no-properties subexp) matches))
    (nreverse matches)))

(defun nvp-regex-all-match-positions (regex bnds subexp)
  "Same parameters as `nvp-regex-all-matches', but gathers match positions."
  (let (positions)
    (nvp-regex:with-matches regex bnds subexp
      (push (cons (match-beginning subexp) (match-end subexp)) positions))
    (nreverse positions)))

;; make indentation based regexp
(defun nvp-indent-regexp ()
  (concat "^\\(?:[ \t]*$\\|"
          (buffer-substring
           (point)
           (save-excursion
             (progn (back-to-indentation) (point))))
          "\\)"))

;; Skip back across `backchars' chars, then look for `forward-regexp',
;; returning cons of start and end of match.
(defun nvp-back-chars-then-look (backchars &optional forward-regexp)
  (or forward-regexp (setq forward-regexp (format "[%s]+" backchars)))
  (save-excursion
    (skip-chars-backward backchars)
    (if (looking-at forward-regexp)
        (cons (point) (match-end 0))
      nil)))


;;; Save data -- FIXME: remove if this not used anywhere?

;; recentf.el
(defun nvp-save-variable (var file &optional header coding)
  "Save the `VAR' into `FILE' with `HEADER' and `CODING'."
  (condition-case error
      (with-temp-buffer
        (erase-buffer)
        (set-buffer-file-coding-system (or coding 'emacs-mule))
        (insert (or header
                    (format-message
                     (eval-when-compile
                       (concat
                        ";; -*- mode: emacs-lisp -*-\n"
                        ";;; Automatically generated on %s.\n"))
                     (current-time-string))))
        (nvp-dump-variable var)
        (insert (concat
                 "\n;; Local\sVariables:\n"
                 ";; no-update-autoloads: t\n"
                 ";; coding: " (or coding 'emacs-mule) "\n"
                 ";; End:\n"
                 ";;; " (file-name-nondirectory file)
                 " ends here\n"))
        (write-file (expand-file-name file))
        (message "Saved %s to %s." var file)
        nil)
    (error
     (warn "%s: %s" var (error-message-string error)))))

(defun nvp-dump-variable (variable)
  "Insert a \"(setq VARIABLE value)\" in the current buffer."
  (let ((value (symbol-value variable)))
    (if (atom value)
        (insert (format "\n(setq %S '%S)\n" variable value))
      (insert (format "\n(setq %S\n      '(" variable))
      (dolist (e value)
        (insert (format "\n        %S" e)))
      (insert "\n        ))\n"))))

(provide 'nvp-util)
;;; nvp-util.el ends here
