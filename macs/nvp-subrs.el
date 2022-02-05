;;; nvp-subrs.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Collection of compile-time subrs -- many of which are used in macros
;; elsewhere in the directory.
;;
;; Note: don't use nvp-macros in here - recursive require
;;; Code:
(require 'cl-lib)
(require 'dash)

;;; Conversion
(defsubst nvp:as-symbol (string-or-symbol)
  (declare (pure t) (side-effect-free t))
  (if (symbolp string-or-symbol) string-or-symbol
    (intern string-or-symbol)))

(defsubst nvp:as-string (string-or-symbol)
  (declare (pure t) (side-effect-free t))
  (if (stringp string-or-symbol) string-or-symbol
    (symbol-name string-or-symbol)))

(defsubst nvp:pair-p (x car-pred cdr-pred)
  "Return non-nil if X is a cons satisfying predicates applied to elems."
  (declare (pure t) (side-effect-free t))
  (and (consp x)
       (funcall car-pred (car x))
       (funcall cdr-pred (cdr x))))

(defsubst nvp:dotted-pair-p (x)
  (declare (pure t) (side-effect-free t))
  (and (consp x) (not (consp (cdr x)))))

(defsubst nvp:as-list (x)
  (declare (pure t) (side-effect-free t))
  (if (and (listp x) (not (functionp x))) x
    (list x)))

(defsubst nvp:unquote (sym)
  (declare (pure t))
  (while (memq (car-safe sym) '(quote function))
    (setq sym (cadr sym)))
  sym)

;; -------------------------------------------------------------------
;;; Hash tests

(defsubst case-fold-string= (a b)
  (eq t (compare-strings a nil nil b nil nil t)))

(defsubst case-fold-string-hash (a)
  (sxhash (upcase a)))

;; case-insensitive hash-table
(define-hash-table-test 'case-fold #'case-fold-string= #'case-fold-string-hash)

;; -------------------------------------------------------------------
;;; Strings

(defsubst nvp:common-prefix (prefix candidates)
  (--> (try-completion prefix candidates)
       (or (and (eq t it) it) it)))

;; Make a string of S repeated NUM times.
(defsubst nvp:s-repeat (num s)
  (let (ss)
    (dotimes (_ num)
      (setq ss (cons s ss)))
    (apply #'concat ss)))

;; If S is shorter than LEN, pad it with CHAR (default spaces) so it's centered.
;; Like `s-center' but allow for CHAR.
(defsubst nvp:s-center (len s &optional char)
  (or char (setq char 32))
  (let ((extra (max 0 (- len (length s)))))
    (concat (make-string (ceiling extra 2) char)
            s
            (make-string (floor extra 2) char))))

;; Format a header with TITLE centered
(defsubst nvp:centered-header (title &optional width char)
  (or width (setq width 85))
  (or char (setq char "~"))
  (let ((len (length title)))
    (format "\n%s\n%s\n\n"
            (nvp:s-center (- width len) title)
            (nvp:s-repeat width char))))

;; -------------------------------------------------------------------
;;; Lists

;; Split LST into N length sublists.
(defsubst nvp:list-split-into-sublists (lst n)
  (declare (pure t) (side-effect-free t))
  (cl-loop for i from 0 to (1- (length lst)) by n
     collect (butlast (nthcdr i lst) (- (length lst) (+ n i)))))

(defsubst nvp:flatten-to-alist (tree)
  "Flatten tree, but leave cons cells. 
The result may also contain atoms that where head of subalists."
  (declare (pure t) (side-effect-free t))
  ;; if ELEM is a list and has a null cdr return its car, otherwise return it
  (cl-macrolet ((flatten-elem (elem)
                  (if (and (consp elem) (null (cdr elem)))
                      (car elem) elem)))
    (let (elems)
      (while (and (consp tree))
        (let ((elem (pop tree)))
          (while (and (consp elem) (consp (cdr elem)))
            (push (cdr elem) tree)
            (setq elem (car elem)))
          (if elem (push (flatten-elem elem) elems))))
      (if tree (push (flatten-elem tree) elems))
      (nreverse elems))))

(define-inline nvp:flatten-tree (lst &optional alist)
  "Flatten nested list. If ALIST is non-nil, leave cons cells intact."
  (declare (pure t) (side-effect-free t))
  (if alist (inline-quote (nvp:flatten-to-alist ,lst))
    (inline-quote (flatten-tree ,lst))))

;; Return longest item by `length'.
(defsubst nvp:longest-item (&rest items)
  (declare (pure t) (side-effect-free t))
  (cl-reduce (lambda (a b) (if (> (length a) (length b)) a b)) items))

(defsubst nvp:listify (&rest args)
  "Ensure all items in ARGS are lists."
  (declare (pure t) (side-effect-free t))
  (mapcar (lambda (arg)
            (and (stringp arg) (setq arg (intern-soft arg)))
            (setq arg (nvp:as-list arg)))
          args))

(defsubst nvp:list-concat (&rest elems)
  "Remove empty lists from ELEMS and append."
  (declare (pure t) (side-effect-free t))
  (apply #'append (delete nil (delete (list nil) elems))))

(defsubst nvp:list-unquote (args)
  "Unquote, unfunction, all elements in args - return as list.
eg. '(#'a b 'c) => '(a b c), or #'fn => '(fn), or ('a #'b) => '(a b)."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe args) '(function quote))
    (setq args (cadr args)))
  (delq nil (if (listp args)
                (cl-loop for arg in args
                   do (while (memq (car-safe arg) '(function quote))
                        (setq arg (cadr arg)))
                   collect arg)
              (cons args nil))))

(defsubst nvp:list-split-at (pred xs)
  "Return list with first element being elements of LST before PRED was non-nil."
  (let ((ys (list nil)) (zs (list nil)) flip)
    (cl-dolist (x xs)
      (if flip (nconc zs (list x))
        (if (funcall pred x)
            (progn
              (setq flip t)
              (nconc zs (list x)))
          (nconc ys (list x)))))
    (cons (cdr ys) (cdr zs))))

(defsubst nvp:list-intersection (lsts)
  "Intersection of all lists in LSTS (unique values)."
  (declare (pure t) (side-effect-free t))
  (cl-reduce #'cl-intersection lsts))

;; -------------------------------------------------------------------
;;; Plists

(defsubst nvp:plist-delete (plist prop)
  (declare (pure t) (side-effect-free t))
  (cl-loop for (k v) on plist by #'cddr
     unless (eq prop k)
     nconc (list k v)))

(defsubst nvp:plist-merge (a b)
  (nconc a (cl-loop for (k v) on b by #'cddr
              if (not (plist-member a k))
              nconc (list k v))))

(defsubst nvp:separate-keywords (lst)
  (let (kws res)
    (while lst
      (if (keywordp (car lst))
          (setq kws (cons (car lst) (cons (cadr lst) kws))
                lst (cddr lst))
        (push (car lst) res)
        (setq lst (cdr lst))))
    (cons kws res)))

;; -------------------------------------------------------------------
;;; Headings / Sections

(defsubst nvp:heading-create-re (&optional comment)
  (declare (side-effect-free t))
  (let* ((comment (or comment (string-trim comment-start)))
         (beg (regexp-quote comment))
         (multi-p (> (string-width comment) 1)))
    (if (not multi-p)
        (format "^\\s-*%s%s\\(?:—\\|---\\|\*\\| |\\|%s\\)\\s-" beg beg beg)
      (format "^\\s-*%s\\(?:—\\|---\\|%s\\)\\s-" beg
              (regexp-quote (substring comment 1 2))))))

;; -------------------------------------------------------------------
;;; Buffers

;; why isn't this a builtin function?
(defsubst nvp:ktb () (kill-buffer (current-buffer)))

;; -------------------------------------------------------------------
;;; Files

(defsubst nvp:ext (&optional path)
  (file-name-extension (or path (buffer-file-name))))
(defsubst nvp:no-ext (&optional path)
  (file-name-sans-extension (or path (buffer-file-name) (buffer-name))))
(defsubst nvp:bfn ()
  (file-name-nondirectory
   (or
    (buffer-file-name)
    (buffer-name))))
(defsubst nvp:bfn-no-ext ()
  (file-name-base
   (or
    (buffer-file-name)
    (buffer-name))))
(defsubst nvp:dfn ()
  (file-name-nondirectory
   (directory-file-name
    (or
     (file-name-directory (file-truename buffer-file-name))
     (file-truename default-directory)))))
(defsubst nvp:fn (&optional path)
  (file-name-nondirectory (directory-file-name (or path (buffer-file-name)))))

;; Locate first name in NAMES using `locate-dominating-file' starting from FILE.
;; I think projectile has a number of functions doing this type of stuff
(defsubst nvp:locate-first-dominating (file names)
  (setq names (nvp:as-list names))
  (cl-loop for name in names
     as res = (locate-dominating-file file name)
     when res
     return (expand-file-name name res)))

;; this must exist somewhere I'm forgetting...
(defsubst nvp:directories (&optional root fullname pattern)
  (or pattern (setq pattern "^[^.]"))
  (--filter (file-directory-p it)
            (directory-files (or root default-directory) fullname pattern)))

;; return list of possible directories containing compile-time libraries
(defconst nvp-compile-time-directories '("compile" "subrs" "macs" "macros"))
(defconst nvp-compile-time-directory-re
  (concat "^" (regexp-opt nvp-compile-time-directories) "\\'"))

(defsubst nvp:compile-time-directories (&optional root full patterns)
  (nvp:directories root full (or patterns nvp-compile-time-directory-re)))

;; -------------------------------------------------------------------
;;; Find

;; path to local notes file or nil
(defsubst nvp:find-notes-file (&optional names)
  (if (and nvp-local-notes-file (file-exists-p nvp-local-notes-file))
      (expand-file-name nvp-local-notes-file)
    (when (and (not nvp-local-notes-file) (derived-mode-p 'comint-mode))
      (hack-local-variables))
    (or names (setq names (or (bound-and-true-p nvp-local-notes-file)
                              nvp-default-notes-files)))
    (let* ((case-fold-search t))
      (setq nvp-local-notes-file
            (nvp:locate-first-dominating
             (or (buffer-file-name) default-directory) names)))))

(defsubst nvp:locate-library (library)
  (--when-let (or (locate-library library)
                  (let ((paths (--map (expand-file-name it nvp/nvp)
                                      '("macs" "subrs" "compile"))))
                    (locate-library (file-name-base library) nil paths)))
    (let ((f (concat (file-name-sans-extension it) ".el")))
      (if (file-exists-p f) f
        (concat f ".gz")))))

;; -------------------------------------------------------------------
;;; I/O

;; add default to prompt in non-nil
(defsubst nvp:prompt-default (prompt &optional default)
  (if default
      (format "%s (default %s): "
              (substring prompt 0 (string-match "[ :]+\\'" prompt)) default)
    prompt))

;; push input back onto command stack
(defsubst nvp:unread (input)
  (cl-loop for c across (reverse input)
           do (push c unread-command-events)))

;; -------------------------------------------------------------------
;;; Syntax

;; Non-nil if POINT is between open/close syntax with only whitespace.
(defsubst nvp:between-empty-parens-p (&optional point)
  (ignore-errors
    (and point (goto-char point))
    (and
     (progn (skip-syntax-forward " ") (eq ?\) (char-syntax (char-after))))
     (progn (skip-syntax-backward " ") (eq ?\( (char-syntax (char-before)))))))

;; -------------------------------------------------------------------
;;; System

;; Numboer of available processors
(define-inline nvp:nproc ()
  (cond
   ((executable-find "nproc")
    (inline-quote
     (string-to-number
      (string-trim (shell-command-to-string "nproc")))))
   ((file-exists-p "/proc/cpuinfo")
    (inline-quote
     (with-temp-buffer
       (insert-file-contents "/proc/cpuinfo")
       (how-many "^processors[[:space:]]+:"))))
   ;; default
   (t (inline-quote 1))))

;; -------------------------------------------------------------------
;;; Process

;; generate and return a new comint buffer
(defsubst nvp:proc-comint-buffer (name)
  (with-current-buffer (generate-new-buffer name)
    (comint-mode)
    (current-buffer)))

;;-- Find processes

;; find first item using TEST function (default 'equal)
(cl-defsubst nvp:proc-find (item &key test key)
  (declare (indent defun))
  (let ((completion-ignore-case t)
        (case-fold-search t))
    (cl-find item (process-list) :test (or test #'equal) :key key)))

;; find first process matched by PRED function
(cl-defsubst nvp:proc-find-if (pred &key key start end from-end)
  (declare (indent defun))
  (and pred (cl-find-if pred (process-list) :key key :start start :end end
                        :from-end from-end)))

;; find all processes matching PRED
(defsubst nvp:proc-find-all (pred)
  (cl-loop for proc in (process-list)
     when (funcall pred proc)
     collect proc))

;; find process by matching NAME
(defsubst nvp:proc-find-by-name (name)
  (nvp:proc-find name :test #'string-match-p :key #'process-name))


(provide 'nvp-subrs)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-subrs.el ends here
