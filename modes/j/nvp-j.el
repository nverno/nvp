;;; nvp-j.el --- from abo-abo config I think -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'j-mode nil t)
(nvp-decl j-console-execute-line j-console-execute-buffer)

;; provided j-atoms
;; (custom-set-faces
;;  '(j-verb-face ((t (:foreground "Red"))))
;;  '(j-adverb-face ((t (:foreground "Green"))))
;;  '(j-conjunction-face ((t (:foreground "Blue"))))
;;  '(j-other-face ((t (:foreground "Black")))))

;; -------------------------------------------------------------------
;;; Utils

(defun nvp-j-inside-comment-p ()
  (let ((beg (nth 8 (syntax-ppss))))
    (and beg (comment-only-p beg (point)))))

;; -------------------------------------------------------------------
;;; J console

(defun nvp-j-execute-line ()
  (interactive)
  (save-buffer)
  (j-console-execute-line)
  (other-window 1))

(defun nvp-j-execute-buffer ()
  (interactive)
  (save-buffer)
  (j-console-execute-buffer)
  (other-window 1))

(defun j-space-wrap (x)
  "Insert X wrapped in single spaces.
Delete all space to the left beforehand."
  (when (looking-back "[^ ]\\( +\\)" (point-min))
    (delete-region (match-beginning 1)
                   (match-end 1)))
  (insert (format " %c " x)))

(defun j-space-right (x)
  "Insert X with single space on the right.
Delete all space to the left beforehand."
  (j-delete-spaces-left)
  (insert (format "%c " x)))

(defun j-space-none (x)
  "Insert X. Delete all space to the left beforehand."
  (j-space-right x)
  (backward-delete-char 1))

(defun j-up ()
  "Move forward out of one level of parentheses or quotes."
  (interactive)
  (unless (ignore-errors (up-list) t)
    (let ((s (syntax-ppss)))
      (when (nth 3 s)
        (goto-char (nth 8 s))
        (forward-sexp)
        (when (looking-back " '" (line-beginning-position))
          (backward-delete-char 2)
          (insert "'")))
      (ignore-errors (up-list))))
  (when (looking-back "\\( +\\))" (point-min))
    (delete-region (match-beginning 1)
                   (match-end 1)))
  (insert " "))

(defun j-from ()
  "J { verb."
  (interactive)
  (j-space-wrap ?{))

(defun j-delete-spaces-left ()
  "Delete all space to the left."
  (when (looking-back " +" (line-beginning-position))
    (delete-region (match-beginning 0)
                   (match-end 0))))

(defun j-wrap-region-parens ()
  "Wrap region with parens."
  (when
      (region-active-p)
    (save-excursion
      (goto-char
       (region-beginning))
      (insert "("))
    (goto-char
     (region-end))
    (insert ")")))

(defun j-ins-parens ()
  "Insert (), taking case of spaces.
Wrap region if it's active."
  (interactive)
  (if (region-active-p)
      (j-wrap-region-parens)
    (j-delete-spaces-left)
    (unless (looking-back "(\\|^\\|'" (line-beginning-position))
      (insert " "))
    (insert "()")
    (backward-char)))

(defun j-join-left (x)
  "Insert X, joined with closest on the left, wrapped in spaces."
  (j-delete-spaces-left)
  (if (looking-back "[^ ]" (point-min))
      (replace-match (format " \\&%c " x))
    (insert (format "%c " x))))

(defvar j-ops-alist
  '((?= (("= " (lambda (x) (j-space-right ?.))) j-space-wrap))
    (?: ((" \\(?:0\\|1\\|2\\|3\\|4\\|13\\|dyad\\)" j-space-wrap)
         ("[$`@=,^-+*%<>;!{}|ipqsux] ?" j-space-right)
         ("_?[0-9] ?" j-space-right)    ; constant verbs
         ("[/~] ?" j-join-left)
         insert))
    (?. (("\\b\\(?:if\\|do\\|else\\|end\\|for\\|while\\|select\\|case\\)" j-space-right)
         ("[-|\\^{}%*+&@#<=>o;,\"!~/] ?" j-space-right) insert))
    (?+ (("(\\|= " j-space-right) j-space-wrap))
    (?- (("(" j-space-right) j-space-wrap))
    (?* (("(" j-space-right)
         ("':" insert) j-space-wrap))
    (?% (("(" j-space-right) j-space-wrap))
    (?< (("(" j-space-right) j-space-wrap))
    (?> (j-space-wrap))
    (?, (j-space-wrap))
    (?^ (j-space-wrap))
    (?o (("1" j-space-wrap) insert))
    (?$ (("#" j-space-right) j-space-wrap))
    (?/ (("[-+*%.=#<] *" j-space-right) insert))
    (?\; (j-space-wrap))
    (?& (("[\\^] " j-space-right) j-space-wrap))
    (?\@ ((")" insert) j-space-wrap))
    (?| (j-space-wrap))
    (?\\ (("[</] ?" j-space-right) insert))
    (?\" (j-space-none))
    (?} (j-space-wrap))
    (?\[ (j-space-wrap))
    (?\] (j-space-wrap))
    (?~ (j-space-right))
    (?` (j-space-wrap))
    (?# (j-space-wrap))
    (?! (j-space-wrap))))

(defun j-setup-shortcuts ()
  "Assign shortcuts for J."
  (interactive)
  (mapc
   (lambda (x) (local-set-key
	   (format "%c" (car x))
	   `(lambda () (interactive) (j-insert ,(car x)))))
   j-ops-alist)
  (mapc
   (lambda (x) (local-set-key (car x) (cadr x)))
   `(("M-C-f" j-up)
     ("M-C-(" j-ins-parens)
     ("M-C-{" j-from)
     (,(kbd "C-c ?") helm-j-cheatsheet)
     (,(kbd "<f2> h f") helm-j-cheatsheet))))

(defun j-insert (x)
  "Insert character X.
Spaces around X are modified accorind to `j-ops-alist'."
  (let (ops)
    (cond ((looking-back "(\\|'" (line-beginning-position))
           (j-space-right x))
          ((nvp-j-inside-comment-p)
           (insert x))
          ((looking-back "^ *" (line-beginning-position))
           (j-space-right x))
          ((setq ops (cadr (assoc x j-ops-alist)))
           (catch 'break
             (dolist (op (butlast ops))
               (if (consp op)
                   (when (looking-back (car op) (line-beginning-position))
                     (throw 'break (funcall (cadr op) x)))))
             (funcall (car (last ops)) x)))
          (t (insert x)))))

(defun j-set-syntax-parens ()
  "Highlight only ()."
  (interactive)
  (modify-syntax-entry ?\' "\"")
  (modify-syntax-entry ?\[ "w")
  (modify-syntax-entry ?\] "w")
  (modify-syntax-entry ?\{ "w")
  (modify-syntax-entry ?\} "w"))

(provide 'nvp-j)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-j.el ends here
