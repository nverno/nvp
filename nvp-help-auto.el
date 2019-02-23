;;; nvp-help-auto.el --- help commands -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-22 23:19:18>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created:  2 February 2019

;;; Commentary:

;; Autoloaded help generic commands
;;; TODO:
;; - list package dependencies: see `package--get-deps'

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'nvp-macro))
(nvp-declare "define-word" define-word define-word-at-point)
(autoload 'nvp-read-keymap "nvp-read")
(autoload 'ispell-get-word "ispell")

;; -------------------------------------------------------------------
;;; Lookup Words

;; Define word at point, with single prefix prompt for word, 
;; with two prefix use lookup-word.
;;;###autoload
(defun nvp-help-define-word (arg)
  "Define word at point, dispatching based on ARG."
  (interactive "p")
  (cond
   ((eq arg 4) (call-interactively  #'define-word))
   ((eq arg 16) (call-interactively #'nvp-help-lookup-word))
   (t (call-interactively           #'define-word-at-point))))

;; Lookup definintion of word at point online.
(defun nvp-help-lookup-word (word)
  (interactive (list (save-excursion (car (ispell-get-word nil)))))
  (browse-url (format "http://en.wiktionary.org/wiki/%s" word)))

;; -------------------------------------------------------------------
;;; Faces 

;; Show the name of face under point.
;;;###autoload
(defun nvp-help-font-face (pos)
  "Info on char face at POS."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face)
      (message "No face at %d" pos))))

;; -------------------------------------------------------------------
;;; Chars
 
;;;###autoload
(defun nvp-help-list-charsets (&optional arg)
  "List names of charsets."
  (interactive "P")
  (if arg
      (list-character-sets nil)
    (let* ((table (mapcar (lambda (x) (list (symbol-name x))) charset-list))
           (charset (ido-completing-read "Charset: " table)))
     (list-charset-chars (intern charset)))))

;;; FIXME: unprintable characters
;;;###autoload
(defun nvp-help-ascii-table ()
  "Print ASCII table values."
  (interactive)
  (nvp-with-results-buffer "*ASCII*"
    (insert "              ASCII               \n"
            "----------------------------------\n")
    (cl-loop
       for i from 0 to 15
       do (cl-loop
             for j from 0 to 15
             as num = (+ i (* j 16))
             do
               (when (= j 0) (insert (format "%4d |" i)))
               (insert (format " %c " num))
               (when (= j 15) (insert "\n"))))))

;; -------------------------------------------------------------------
;;; Bindings

;;;###autoload
(defun nvp-help-describe-bindings (prefix)
  "Describe bindings beginning with PREFIX."
  (interactive (list (read-string "Bindings prefix (enter as for 'kbd'): ")))
  (describe-bindings (kbd prefix)))

;;;###autoload
(defun nvp-help-describe-keymap (keymap)
  "Describe KEYMAP readably."
  (interactive (list (nvp-read-keymap)))
  (setq keymap (or (ignore-errors (indirect-variable keymap)) keymap))
  (help-setup-xref (list #'nvp-help-describe-keymap keymap)
                   (called-interactively-p 'interactive))
  (let ((name (symbol-name keymap))
        (doc (documentation-property keymap 'variable-documentation)))
    (and (equal "" doc) (setq doc nil))
    (with-help-window (help-buffer)
      (princ name) (terpri) (princ (make-string (length name) ?-)) (terpri) (terpri)
      (with-current-buffer standard-output
        (when doc
          (princ doc) (terpri) (terpri))
        ;; see https://www.emacswiki.org/emacs/help-fns%2b.el
        ;; Use `insert' instead of `princ', so control chars (e.g. \377) insert
        ;; correctly.
        (insert (substitute-command-keys (concat "\\{" name "}")))))))

;;; https://www.emacswiki.org/emacs/help-fns%2b.el
;; describe-package ?

(provide 'nvp-help-auto)
;;; nvp-help-auto.el ends here
