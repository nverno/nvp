;;; nvp-dev.el --- elisp devel helpers -*- lexical-binding: t; -*-

;; Last modified: <2019-03-08 23:44:58>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 14 February 2019

;;; Commentary:

;; TODO:
;; - function to remove all methods from generic
;; - how to remove all notifications (filenotify) without storing them?
;; - activate ert when in test buffer, otherwise try project build test?

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'nvp-display)
(require 'help-mode)
(nvp-declare "nadvice" advice-mapc advice-remove)
(nvp-declare "ert" ert-run-tests-interactively)
(nvp-autoload "nvp-string" nvp-s-wrap nvp-s-center nvp-s-repeat)

(define-button-type 'help-marker
  :supertype 'help-xref
  'help-function (lambda (m) (pop-to-buffer (marker-buffer m)) (goto-char m))
  'help-echo (purecopy "mouse-2, RET: go to this marker"))

;;;###autoload
(defun nvp-ert-run-tests ()
  "Run ert tests.
With prefix ARG, prompt for selector."
  (interactive)
  (if (not (featurep 'ert))
      (user-error "`ert' must be loaded to run this function")
    (eval-buffer (current-buffer))
    (call-interactively 'ert-run-tests-interactively)))

;;;###autoload
(defun nvp-advice-remove-all (sym)
  "Remove all advice from SYM."
  (interactive "aFunction: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

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

(defun nvp-describe-key-events (&optional arg)
  (interactive "P")
  (nvp-display-buffer-with-action 4
    (with-help-window (help-buffer)
        (princ
         (format "\n%s\n%s\n\n"
                 (nvp-s-center 60 "Key commands")
                 (nvp-s-repeat 85 "~")))
        (let ((vars '(this-command
                      real-this-command
                      this-original-command
                      last-command
                      last-command-event
                      last-input-event
                      last-repeatable-command
                      last-event-frame
                      current-prefix-arg
                      prefix-arg
                      last-prefix-arg
                      ))))
      (with-current-buffer standard-output
        (let ((inhibit-read-only t))
          (hl-line-mode))))))

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

(provide 'nvp-dev)
;;; nvp-dev.el ends here
