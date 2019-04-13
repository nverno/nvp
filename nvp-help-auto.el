;;; nvp-help-auto.el --- help commands -*- lexical-binding: t; -*-

;;; Commentary:

;; Autoloaded help generic commands
;;; TODO:
;; - list package dependencies: see `package--get-deps'

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'nvp-macro))
(require 'nvp)
(nvp-declare define-word define-word-at-point)
(autoload 'nvp-read-keymap "nvp-read")
(autoload 'ispell-get-word "ispell")

;; -------------------------------------------------------------------
;;; Words

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

;; `list-block-of-chars' function does legwork
;;;###autoload
(defun nvp-help-list-charsets (&optional arg)
  "List names of charsets."
  (interactive "P")
  (if arg
      (list-character-sets nil)
    (let* ((table (mapcar (lambda (x) (list (symbol-name x))) charset-list))
           (charset (nvp-completing-read "Charset: " table)))
     (list-charset-chars (intern charset)))))

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
