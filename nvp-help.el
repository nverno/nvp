;;; nvp-help.el --- help commands -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'transient)
(require 'nvp)
(nvp:decls :f (dictionary-search dictionary-match-words powerthesaurus-lookup-dwim))
(nvp:auto "nvp-read" 'nvp-read-keymap)
(nvp:auto "nvp-edit" 'nvp-sort-lines-first-symbol)
(nvp:auto "ispell" 'ispell-get-word)

;;;###autoload
(defun nvp-push-button (&optional same-window)
  (interactive "P")
  (let ((display-buffer-overriding-action
         (when same-window
           '(display-buffer-same-window
             ((inhibit-switch-frame . t)
              (inhibit-same-window  . nil))))))
    (call-interactively #'push-button)))

;;;###autoload
(defun nvp-push-button-and-go (&optional n)
  "Push the Nth button and stay in same frame.
Ignore buttons like `native-comp-function',`primitive-function'."
  (interactive "p")
  (setq n (if n (abs n) 1))
  (cl-flet ((good-p (pos)
              (and-let* ((btn (button-at pos)))
                (not (member (button-get btn 'help-args)
                             '((native-comp-function)
                               (primitive-function)
                               (interpreted-function)
                               (byte-code-function)))))))
    (let* ((pos (point))
           (beg pos)
           (lim (point-max))
           (cnt 0))
      (when (good-p pos)
        (cl-decf n)
        (cl-incf cnt))
      (while (> n 0)
        (setq pos (next-button pos))
        (and (overlayp pos)
             (setq pos (overlay-start pos)))
        (if (null pos)
            (setq pos (point-min)
                  lim beg)
          (when (> pos lim)
            (if (zerop cnt)
                (user-error "no buttons")
              (setq n (mod n cnt)
                    lim (point-max))))
          (when (good-p pos)
            (cl-decf n)
            (cl-incf cnt))))
      (cl-assert (button-at pos) t)
      (goto-char pos)
      (nvp-push-button 'same-window))))

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
      (princ name) (terpri)
      (princ (make-string (length name) ?-))
      (terpri) (terpri)
      (with-current-buffer standard-output
        (when doc
          (princ doc) (terpri) (terpri))
        ;; see https://www.emacswiki.org/emacs/help-fns%2b.el
        ;; Use `insert' instead of `princ', so control chars (e.g. \377) insert
        ;; correctly.
        (insert (nvp-help--sort-keymap name))))))

;; -------------------------------------------------------------------
;;; Transient

(nvp:decl-prefixes devdocs dash hyperpolyglot cheatsheet lookup cheat)
(nvp:auto "nvp-devdocs-config" nvp-devdocs-list-docsets)
(nvp:auto "powerthesaurus"powerthesaurus-transient)

;;;###autoload(autoload 'nvp-devdocs-menu "nvp-help" nil t)
(transient-define-prefix nvp-devdocs-menu ()
  [["Search"
    ("s" "Search in active docs" devdocs-lookup)
    ("S" "Search DevDocs website" devdocs-search)
    ("d" "Goto doc (1st page)" devdocs-peruse)
    ;; ("c" "Choose docs to search in" devdocs-browser-open-in)
    ]
   ["Docsets"
    ("i" "Install" devdocs-install)
    ("r" "Uninstall" devdocs-delete)
    ("u" "Upgrade all" devdocs-update-all)
    ("l" "List installed/active" nvp-devdocs-list-docsets)
    ;; ("U" "Upgrade all docs" devdocs-browser-upgrade-all-docs)
    ;; ("m" "Update metadata" devdocs-browser-update-docs)
    ]]
  (interactive)
  (transient-setup 'nvp-devdocs-menu))

;;;###autoload(autoload 'nvp-help-menu "nvp-help" nil t)
(transient-define-prefix nvp-help-menu ()
  "Help"
  [["Goto Definition"
    ("f" "Function" find-function)
    ("v" "Variable" find-variable)
    ("K" "Function on key" find-function-on-key)]
   ["Documentation"
    ("n" "Noman" noman :if (lambda () (fboundp 'noman)))
    ("m" "Man" man)
    ("M" "Consult man" consult-man)
    ("i" "Info" nvp-info-menu)
    ("d" "Devdocs" nvp-devdocs-menu :transient transient--do-replace
     :if (lambda () (fboundp 'devdocs-lookup)))]
   ["Cheat.sh"
    ("cc" "Search" cheat-sh)
    ("cl" "List" cheat-sh-list)]
   ["External"
    ("C" "Cheatsheet Lookup" cheatsheet-lookup)
    ("l" "Lookup-help links" lookup-help)
    ("p" "Hyperpolyglot" hyperpolyglot)]]
  [["Words/Numbers"
    ("N" "Number" nvp-number-menu :transient transient--do-replace)
    ("w" "Word dwim" nvp-help-word-dwim)
    
    ("S" "Spellcheck" ispell)]
   ["Fonts/Faces/Charsets"
    ("F" "Font menu" nvp-font-menu :transient transient--do-replace)]
   ["Libs"
    (":sp" "Smartparens Cheatsheet" sp-cheat-sheet :if-non-nil smartparens-mode)]])

;;; Optional Menus
(when (fboundp 'dash-docs-activate-docset)
  (transient-append-suffix 'nvp-help-menu '(0 1 4)
    '("D" "Dash" nvp-dash-menu :transient transient--do-replace)))

(when (fboundp 'powerthesaurus-transient)
  (transient-append-suffix 'nvp-help-menu '(1 0 1)
    '("W" "Powerthesaurus" powerthesaurus-transient
      :transient transient--do-replace)))

(provide 'nvp-help)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-help.el ends here
