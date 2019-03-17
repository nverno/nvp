;;; nvp-dev.el --- elisp devel helpers -*- lexical-binding: t; -*-

;; Last modified: <2019-03-16 21:31:45>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 14 February 2019

;;; Commentary:

;; TODO:
;; - function to remove all methods from generic
;; - how to remove all notifications (filenotify) without storing them?

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'nvp-display)
(require 'help-mode)
(nvp-declare "nadvice" advice-mapc advice-remove)
(nvp-autoload "nvp-string" nvp-s-wrap nvp-s-center nvp-s-repeat)
(defvar nvp-mode-cache)

(define-button-type 'help-marker
  :supertype 'help-xref
  'help-function (lambda (m) (pop-to-buffer (marker-buffer m)) (goto-char m))
  'help-echo (purecopy "mouse-2, RET: go to this marker"))

;;;###autoload
(defun nvp-advice-remove-all (sym)
  "Remove all advice from SYM."
  (interactive "aFunction: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

;;;###autoload
(defun nvp-dev-make-and-reload ()
  "Make and reload package autoloads."
  (interactive)
  (call-process "make" nil 0 nil "-k")
  (let ((file (car (directory-files (expand-file-name ".") t "autoloads.el"))))
    (load-file file)))

;; (defun nvp-dev-mode-cache (mode)
;;   "Examine, refresh MODE's config cache.
;; With prefix, examine contents instead of resetting them."
;;   (interactive (list (intern (completing-read "Mode: " nvp-mode-cache))))
;;   (require 'nvp-setup)
;;   (if current-prefix-arg
;;       (let ((cols 5))
;;         (nvp-with-view-list
;;           :name "mode-cache"
;;           :mode-name "Mode Cache"
;;           :format
;;           (cl-loop for slot across 'nvp-mode-vars
;;              do (message "%S" slot))
;;           (mapcar #'symbol-name (cdr (cl-struct-slot-info 'nvp-mode-vars)))
;;           [("Dir" 12) ("Snippets" 12) ("Abbr-file" 12) ("Abbr-table" 12)]
;;           :entries
;;           (car (cl-struct-slot-info 'nvp-mode-vars))
;;           (cl-loop repeat cols
;;                vconcat [("")])))
;;       (remhash mode nvp-mode-cache)))

;; (defmacro nvp-with-struct-slots (spec-list struct &rest body)
;;   (macroexp-let2 nil struct struct
;;     (let* ((descs (cl-struct-slot-info struct))
;;            (type (pop descs)))
;;      `(cl-symbol-macrolet
;;           ,(mapcar (lambda (entry)
;;                      (let ((var (if (listp entry) (car entry) entry))
;;                            (slot (if (listp entry (cadr entry) entry))))
;;                        (list var ))))))))

;; https://github.com/abo-abo/oremacs
;;;###autoload
(defun nvp-dev-describe-hash (variable)
  "Display the full documentation of VARIABLE (a symbol)."
  (interactive
   (let ((v (variable-at-point))
         (enable-recursive-minibuffers t)
         val)
     (setq val (completing-read
                (if (and (symbolp v)
                         (hash-table-p (symbol-value v)))
                    (format
                     "Describe hash-map (default %s): " v)
                  "Describe hash-map: ")
                obarray
                (lambda (atom) (and (boundp atom)
                               (hash-table-p (symbol-value atom))))
                t nil nil
                (if (hash-table-p v) (symbol-name v))))
     (list (if (equal val "") v (intern val)))))
  (with-output-to-temp-buffer (help-buffer)
    (maphash (lambda (key value)
               (pp key)
               (princ " => ")
               (pp value)
               (terpri))
             (symbol-value variable))))

;; -------------------------------------------------------------------
;;; Syntax

;;;###autoload
(defun nvp-syntax-at-point (marker &optional action)
  "Display info about syntax at point.
With prefix, display in same frame using `display-buffer' ACTION."
  (interactive (list (point-marker) (prefix-numeric-value current-prefix-arg)))
  ;; (set-buffer (marker-buffer marker))
  (let ((ppss (syntax-ppss marker))
        (help
         '("depth in parens."
           "character address of start of innermost containing list; nil if none."
           "character address of start of last complete sexp terminated."
           "non-nil if inside a string. \
(it is the character that will terminate the string, \
or t if the string should be terminated by a generic string delimiter.)"
           "nil if outside a comment, t if inside a non-nestable comment, \
else an integer (the current comment nesting)."
           "t if following a quote character."
           "the minimum paren-depth encountered during this scan."
           "style of comment, if any."
           "character address of start of comment or string; nil if not in one."
           "List of positions of currently open parens, outermost first."
           "When the last position scanned holds the first character of a \
(potential) two character construct, the syntax of that position, \
otherwise nil.  That construct can be a two character comment \
delimiter or an Escaped or Char-quoted character."
           ".... Possible further internal information used by \
‘parse-partial-sexp’.")))
    (help-setup-xref (list #'nvp-syntax-at-point marker)
                     (called-interactively-p 'interactive))
    (nvp-display-buffer-with-action action
      (with-help-window (help-buffer)
        (princ
         (format "\n%s\n%s\n\n"
                 (nvp-s-center 60 "Syntax at <marker>")
                 (nvp-s-repeat 85 "~")))
      
       (cl-loop
          for i from 0 upto (length ppss)
          do
            (princ (format "%d) %S " i (nth i ppss)))
            (princ (format "%s" (nvp-s-wrap 45 (nth i help) "; ")))
            (terpri))
       (with-current-buffer standard-output
         (let ((inhibit-read-only t)
               (comment-start "; ")
               (fill-column 85)
               (comment-column 30))
           (goto-char (point-min))
           (search-forward "<marker>")
           (replace-match "")
           (help-insert-xref-button (format "%S" marker) 'help-marker marker)
           (forward-line 2)
           (while (not (eobp))
             (when (looking-at-p comment-start)
               (insert "|"))
             (comment-indent)
             (forward-line 1)))
         (hl-line-mode))))))

;; -------------------------------------------------------------------
;;; Keys

;; (defun nvp-describe-key-events (&optional arg)
;;   (interactive "P")
;;   (nvp-display-buffer-with-action 4
;;     (with-help-window (help-buffer)
;;         (princ
;;          (format "\n%s\n%s\n\n"
;;                  (nvp-s-center 60 "Key commands")
;;                  (nvp-s-repeat 85 "~")))
;;         (let ((vars '(this-command
;;                       real-this-command
;;                       this-original-command
;;                       last-command
;;                       last-command-event
;;                       last-input-event
;;                       last-repeatable-command
;;                       last-event-frame
;;                       current-prefix-arg
;;                       prefix-arg
;;                       last-prefix-arg
;;                       ))))
;;       (with-current-buffer standard-output
;;         (let ((inhibit-read-only t))
;;           (hl-line-mode))))))

;; -------------------------------------------------------------------
;;; Assorted

;; Print counts of strings in region, with prefix dump at point
;;;###autoload
(defun nvp-stats-uniq (beg end &optional count-lines)
  "Print counts (case-insensitive) of unique words in region BEG to END.
With prefix COUNT-LINES count unique lines."
  (interactive "r\nP")
  (require 'nvp-hash)
  (let ((ht (make-hash-table :test 'case-fold))
        (lines (split-string
                (buffer-substring-no-properties beg end) "\n" 'omit-nulls " "))
        lst)
    (if count-lines
        (dolist (line lines)
          (puthash line (1+ (gethash line ht 0)) ht))
      ;; strip punctuation for words
      (cl-loop for line in lines
         as words = (split-string line "[[:punct:] \t]" 'omit " ")
         when words
         do (cl-loop for word in words
               do (puthash word (1+ (gethash word ht 0)) ht))))
    (maphash (lambda (key val) (push (cons val key) lst)) ht)
    (setq lst (cl-sort lst #'> :key #'car))
    (nvp-with-results-buffer nil
      (pcase-dolist (`(,k . ,v) lst)
        (princ (format "%d: %s\n" k v))))))

;; -------------------------------------------------------------------
;;; Unused

;; (autoload 'nvp-env-substitute-vars "nvp-env")

;; Read config filename lines, expanding environment variables in key-value pairs
;; key-value pairs are separated by SEPARATORS and value may be quoted
;; lines beginning with COMMMENTS regex are ignored
;; separators default to ":=" and comments default to '#'
;; Return list of (key . value) pairs
; (defun nvp-config-read-file (filename &optional separators comments)
;   (setq separators (regexp-quote (or separators ":=")))
;   (setq comments (regexp-quote (or comments "#")))
;   (with-temp-buffer
;     (insert-file-contents filename)
;     (goto-char (point-min))
;     (let ((key-val-regex
;            (concat "^\\([^" separators "\n]+\\)[" separators "]+\\([^\n]+\\)"))
;           (vars))
;       (while (not (eobp))
;         (when (and (not (looking-at-p comments))
;                    (looking-at key-val-regex))
;           ;; expand enviroment variables and remove quotes from values
;           (push (cons (string-trim (match-string-no-properties 1))
;                       (nvp-env-substitute-vars
;                        (match-string-no-properties 2) 'unquote))
;                 vars))
;         (forward-line 1))
;       vars)))

(provide 'nvp-dev)
;;; nvp-dev.el ends here
