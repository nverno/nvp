;;; nvp-help.el --- help commands -*- lexical-binding: t; -*-

;;; Commentary:

;; Autoloaded help generic commands
;;; TODO:
;; - list package dependencies: see `package--get-deps'

;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp-decl "define-word" define-word define-word-at-point)
(nvp-auto "nvp-read" 'nvp-read-keymap)
(nvp-auto "nvp-edit" 'nvp-sort-lines-first-symbol)
(nvp-auto "ispell" 'ispell-get-word)

;;;###autoload
(defun nvp-push-button (&optional same-window)
  (interactive "P")
  (if same-window
      (let ((display-buffer-overriding-action
             '(display-buffer-same-window
               ((inhibit-switch-frame . t)
                (inhibit-same-window  . nil)))))
        (call-interactively #'push-button))
    (call-interactively #'push-button)))

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
  (let ((face (if current-prefix-arg
                  (read-face-name "Face: ")
                (or (get-char-property (point) 'read-face-name)
                    (get-char-property (point) 'face)
                    (read-face-name "Face: ")))))
    (describe-face face)
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

(defsubst nvp-help--sort-keymap (map-name)
  (with-temp-buffer
    (insert (substitute-command-keys (format "\\{%s\}" map-name)))
    (goto-char (point-min))
    (forward-line 3)                    ;to first key def. line
    (let ((beg (point))                 ;sort first group of keys
          (end (progn (re-search-forward "^\\s-*$" nil t) (point))))
      (nvp-sort-lines-first-symbol beg end))
    (buffer-string)))

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
        (insert (nvp-help--sort-keymap name))))))

;;; https://www.emacswiki.org/emacs/help-fns%2b.el
;; describe-package ?

(provide 'nvp-help)
;;; nvp-help.el ends here
