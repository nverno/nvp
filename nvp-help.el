;;; nvp-help.el --- help commands -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Autoloaded help generic commands
;;; TODO:
;; - list package dependencies: see `package--get-deps'
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp:decls :f (dictionary-search dictionary-match-words powerthesaurus-lookup-dwim))
(nvp:auto "nvp-read" 'nvp-read-keymap)
(nvp:auto "nvp-edit" 'nvp-sort-lines-first-symbol)
(nvp:auto "ispell" 'ispell-get-word)

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

;;;###autoload
(defun nvp-push-button-and-go (&optional n)
  "Push the Nth button and stay in same frame."
  (interactive "p")
  (or n (setq n 1))
  (cl-block nil
    (goto-char (point-min))
    (while (and (> n 0) (< (point) (point-max)))
      (goto-char (next-char-property-change (point)))
      (cl-decf n)
      (if (and (= n 0) (get-text-property (point) 'button))
          (cl-return (nvp-push-button 'same-window))))))

;; -------------------------------------------------------------------
;;; Words

;; Define word at point, with single prefix prompt for word, 
;; with two prefix use lookup-word.
;;;###autoload
(defun nvp-help-word-dwim (arg)
  "Lookup word in dictionary/thesaurus, dispatch on prefix:
(nil)	 search for word
\\[universal-argument]	 pattern search
\\[universal-argument] \\[universal-argument]\
	lookup word with `powerthesaurus', if available, or on wiktionary."
  (interactive "p")
  (cond
   ((eq arg 4) (call-interactively  #'dictionary-match-words))
   ((eq arg 16) (call-interactively #'nvp-help-lookup-word-external))
   (t (call-interactively           #'dictionary-search))))

;; Lookup definintion of word at point online.
(defun nvp-help-lookup-word-external ()
  (interactive)
  (if (or (bound-and-true-p powerthesaurus-lookup-dwim)
          (require 'powerthesaurus nil t))
      (funcall-interactively #'powerthesaurus-lookup-dwim)
    (browse-url (format "http://en.wiktionary.org/wiki/%s"
                        (save-excursion (car (ispell-get-word nil)))))))

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
(defun nvp-help-display-charset (&optional arg)
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
;; -------------------------------------------------------------------
;;; Transient

(require 'transient)
(nvp:decl hyperpolyglot cheatsheet-lookup lookup-help cheat-sh cheat-sh-list
  dash-docs-install dash-docs-activate-docset dash-docs-deactivate-docset)

;;;###autoload(autoload 'nvp-help-menu "nvp-help")
(transient-define-prefix nvp-help-menu ()
  "Help"
  [["Goto Definition"
    ("f" "Function" find-function)
    ("v" "Variable" find-variable)
    ("K" "Function on key" find-function-on-key)]
   ["Documentation"
    ("m" "Man" man)
    ("i" "Info" nvp-info-menu)
    ("d" "Dash" consult-dash)
    ("M" "Consult man" consult-man)]
   ["Cheat.sh"
    ("cc" "Search" cheat-sh)
    ("cl" "List" cheat-sh-list)]
   ["External"
    ("ec" "Cheatsheet Lookup" cheatsheet-lookup)
    ("el" "Lookup-help links" lookup-help)
    ("eh" "Hyperpolyglot" hyperpolyglot)]]
  [["Words"
    ("w" "Lookup Dwim" nvp-help-word-dwim)]
   ;; TODO: simplify+remove
   ["Numbers"
    ("nt" "Toggle Base" nvp-number-toggle-base)
    ("nh" "Hex string to Int" nvp-number-hex-string-to-integer)
    ("no" "Octal string to Int" nvp-number-octal-string-to-integer)]
   ["Manage Dash"
    ("Di" "Dash Install" nvp-dash-docs-install)
    ("Da" "Dash Activate" dash-docs-activate-docset)
    ("Dd" "Dash Deactivate" dash-docs-deactivate-docset)]
   ["Libs"
    ;; (":sos" "Sos Keybindings" nvp-sos)
    (":sp" "Smartparens Cheatsheet" sp-cheat-sheet :if-non-nil smartparens-mode)]])

(provide 'nvp-help)
;;; nvp-help.el ends here
