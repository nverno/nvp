;;; nvp-macs-fonts.el --- Extra font-locking stuff -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; References:
;; - see cc-fonts.el `c-font-lock-doc-comments', and
;;   constant `gtkdoc-font-lock-doc-comments' for usage
;;
;; TODO:
;; - mark comment as doc comment based on a match -- not as restrictive as
;;   doxygen, eg. don't require a leading marker
;; - Apply matchers inside marked doc comment
;;
;;; Code:
(require 'nvp-macs-common)
(require 'cc-defs)
(require 'cc-fonts)
(defvar nvp-mode-font-additions)

;; return start of string or comment surrounding point or nil if not in one
(defun nvp:-font-start-of-string-or-comment (&optional syntax)
  (let ((s (or syntax (parse-partial-sexp (point-min) (point)))))
    (and (or (nth 3 s)
             (and (nth 4 s) (not (eq (nth 7 s) 'syntax-table))))
         (nth 8 s))))

;; Non-nil if point is inside a CHAR quoted region
(defsubst nvp:font-lock-quoted-p (&optional char)
  (eq (nth 3 (syntax-ppss)) (or char ?\")))

;; add fontification to REGEX up to LIMIT in quoted area by CHAR
;; (default double-quotes)
(defun nvp:-fontify-quoted-1 (regex char)
  `(lambda (limit)
     (let (res)
       (while (and (setq res (re-search-forward ,regex limit 'move))
                   (not (nvp:font-lock-quoted-p ,char))))
       res)))

;;; TODO: use this in newline-dwim when inside strings and wanting an
;;        escape to the next line
;; stackoverflow
(defun syntax:-to-char (syntax-code syntax-table)
  "Return chars (or char range) with SYNTAX-CODE in SYNTAX-TABLE."
  (let (result)
    (map-char-table
     (lambda (k v)
       ;; k is either a char or (FROM . TO)
       ;; v is (SYNTAX-CODE . MATCHING-CHAR)
       (when (= (car v) syntax-code)
         (push (pcase k
                 (`(,from . ,to) (list (string from) (string to)))
                 (_ (string k)))
               result)))
     syntax-table)
    (nreverse result)))

(defun nvp:-font-lock-doc-comments (prefix limit keywords)
  ;; Simplified `c-font-lock-doc-comments' (cc-fonts)
  ;; Differences:
  ;; - doesn't match sequences of doc comments
  ;; - whole comment blocks are considered doc blocks if any line matches PREFIX
  ;; - doesn't look for trailing C-style "*/"
  ;;
  ;; Blocks contain a line b/w point and LIMIT
  ;; whose start matches PREFIX with `c-doc-face-name'
  ;; Assumes comments have already been fontified with `font-lock-comment-face'.
  ;; Returns nil always.
  ;;
  ;; After fontification of matching comment, fontification according to
  ;; KEYWORDS is applied inside of comment -- like `font-lock-keywords', except
  ;; anchored matches and eval clauses aren't supported
  ;;
  ;; Buffer is narrowed to the comment while KEYWORDS is applied; leading
  ;; comment enders for block commment are not.
  ;;
  ;; This function might do hidden buffer changes.

  (let (comment-beg region-beg)
    (if (memq (get-text-property (point) 'face) '(font-lock-comment-face
                                                  font-lock-comment-delimiter-face))
        ;; Case when fontified region starts inside a comment.
        (let ((start (nvp:-font-start-of-string-or-comment)))
          (setq region-beg (point))
          (when start
            (goto-char start))
          (when (looking-at prefix)
            (setq comment-beg (point)))))
    (while (or
            comment-beg
            
            ;; search for the prefix until a match is found at the start of a comment
            (while (when (re-search-forward prefix limit t)
                     (setq comment-beg (match-beginning 0))
                     (or (not (c-got-face-at comment-beg c-literal-faces))
                         (and (/= comment-beg (point-min))
                              (c-got-face-at (1- comment-beg) c-literal-faces))))
              (setq comment-beg nil))
            (setq region-beg comment-beg))

      ;; differs here -- doesn't collect sequences of doc style line comments
      (goto-char comment-beg)
      (c-forward-single-comment)
      (c-put-font-lock-face comment-beg (point) c-doc-face-name))
    (if (> (point) limit) (goto-char limit))
    (setq comment-beg nil)

    (let ((region-end (point))
          (keylist keywords) keyword matcher highlights)
      (save-restriction
        ;; Narrows to the doc comment.  Among other things, this helps by making
        ;; "^" match at the start of the comment.
        ;; Does not include a trailing block comment ender, though.
        (narrow-to-region region-beg region-end)

        (while keylist
          (setq keyword (car keylist)
                keylist (cdr keylist)
                matcher (car keyword))
          (goto-char region-beg)
          (while (if (stringp matcher)
                     (re-search-forward matcher region-end t)
                   (funcall matcher region-end))
            (setq highlights (cdr keyword))
            (if (consp (car highlights))
                (while highlights
                  (font-lock-apply-highlight (car highlights))
                  (setq highlights (cdr highlights)))
              (font-lock-apply-highlight highlights))))

        (goto-char region-end))))
  nil)
(put 'nvp:-font-lock-doc-comments 'lisp-indent-function 2)

;; -------------------------------------------------------------------
;;; Adding font-lock additions

(cl-defmacro nvp:fontify-quoted (&rest forms &key char &allow-other-keys)
  "Fontify elements in quoted regions."
  (declare (indent defun) (debug t))
  (while (keywordp (car forms))
    (setq forms (cdr (cdr forms))))
  (nvp:defq char ?\")
  (macroexp-progn
   (cl-loop for (regex font) in forms
      collect `(cons
                ,(nvp:-fontify-quoted-1 regex char)
                ',font))))

(defmacro nvp:font-lock-keywords (&rest forms)
  "Create list of font-lock additions.
Each element of FORMS is a list ([:quoted char] regex font-spec)."
  (declare (indent defun) (debug t))
  `(append
    ,@(cl-loop for form in forms
         as quoted = (plist-get form :quoted)
         as splice = (plist-get form :splice)
         when (consp form)
         do (while (keywordp (car form))
              (if (memq (car form) '(:splice)) ; ill-formed plist keys
                  (setq form (cdr form))
                (setq form (cdr (cdr form)))))
         if (and splice (consp form))
         collect (car form)
         else
         ;; collect `,(car form)
         ;; else
         if quoted
         collect `(list (nvp:fontify-quoted :char ,quoted ,form))
         else
         collect (if (consp form)
                     `(list (cons ,(car form) ',(cdr form)))
                   `(list ,form)))))

(defmacro nvp:font-lock-add-defaults (mode &rest forms)
  "Add font-lock additions to MODE."
  (declare (indent defun))
  (macroexp-let2 nil fonts `(progn (nvp:font-lock-keywords ,@forms))
   `(progn
      (cl-pushnew (cons ,mode ,fonts) nvp-mode-font-additions :test #'equal)
      (font-lock-add-keywords ,mode ,fonts))))

(provide 'nvp-macs-fonts)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-macs-fonts.el ends here
