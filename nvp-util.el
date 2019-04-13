;;; nvp-util.el --- Various utility functinos -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'nvp)

;; -------------------------------------------------------------------
;;; Lists

(defun nvp-list-flatten (lst)
  "Flatten nested list."
  (declare (pure t) (side-effect-free t))
  (if (and (listp lst) (listp (cdr lst)))
      (apply #'append (mapcar (lambda (x) (nvp-list-flatten x)) lst))
    (list lst)))

;; Intersection of multiple lists.
(defun nvp-list-intersection (l)
  (declare (pure t) (side-effect-free t))
  (cond ((null l) nil)
	((null (cdr l)) (car l))
	(t (cl-intersection (car l) (nvp-list-intersection (cdr l))))))

;; Split list LST into N sublists.
(defun nvp-list-split (lst n)
  (cl-loop for i from 0 to (1- (length lst)) by n
     collect (butlast (nthcdr i lst) (- (length lst) (+ n i)))))

;; -------------------------------------------------------------------
;;; Regexp

(eval-when-compile
  (defmacro nvp--regex-set-defaults (&optional bnds thing subexp)
    `(progn
       (or ,bnds (setq ,bnds (nvp-tap 'btap ,thing))
           (user-error "No region given to search in."))
       (or ,subexp (setq ,subexp 0)))))

(defun nvp-regex-map-across-matches (fun regex &optional bnds thing subexp)
  "Apply FUN to all REGEX matches in BNDS or bounds of THING.
If non-nil use SUBEXP regexp group."
  (nvp--regex-set-defaults bnds thing subexp)
  (save-match-data
    (let ((pos (car bnds))
          (end (cdr bnds)))
      (save-excursion
        (goto-char pos)
        (while (and (< pos end)
                    (re-search-forward regex end 'move))
          (setq pos (1+ (match-beginning subexp)))
          (funcall fun (match-string subexp)))))))

(defun nvp-regex-all-matches (regex &optional bnds thing subexp)
  "Find all matches of REGEX w/in region BNDS or bounds of THING at point.
Regex matches are collected for SUBEXP (default 0)."
  (nvp--regex-set-defaults bnds thing subexp)
  (save-match-data
    (let ((pos (car bnds))
          (end (cdr bnds))
          matches)
      (save-excursion
        (goto-char pos)
        (while (and (< pos end)
                    (re-search-forward regex end 'move))
          (setq pos (1+ (match-beginning subexp)))
          (push (match-string-no-properties subexp) matches)))
      (nreverse matches))))

(defun nvp-regex-all-match-positions (regex &optional bnds thing subexp)
  "Same parameters as `nvp-regex-all-matches', but gathers match positions."
  (nvp--regex-set-defaults bnds thing subexp)
  (save-match-data
    (let ((pos (car bnds))
          (end (cdr bnds))
          positions)
      (save-excursion
        (goto-char pos)
        (while (and (< pos end)
                    (re-search-forward regex end 'move))
          (setq pos (1+ (match-beginning subexp)))
          (push (cons (match-beginning subexp) (match-end subexp)) positions)))
      (nreverse positions))))

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

;; -------------------------------------------------------------------
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
