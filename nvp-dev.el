;;; nvp-dev.el --- elisp devel helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; TODO:
;; - function to remove all methods from generic
;; - how to remove all notifications (filenotify) without storing them?

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-results)
  (require 'nvp-display))
(require 'nvp)
(require 'help-mode)
(nvp-decl "nadvice" advice-mapc advice-remove)
(nvp-auto "nvp-util" nvp-s-wrap)
(nvp-auto "nvp-read" 'nvp-read-elisp-variable)
(nvp-auto "s" 's-split-words)
(nvp-decl nvp-read-mode)
(nvp-decls)

(define-button-type 'help-marker
  :supertype 'help-xref
  'help-function (lambda (m) (pop-to-buffer (marker-buffer m)) (goto-char m))
  'help-echo (purecopy "mouse-2, RET: go to this marker"))

(nvp-bindings nvp-dev-keymap nil
  :create t
  ("a"  . nvp-dev-advice-remove-all)
  ("c"  . nvp-help-list-charsets)
  ("D"  . describe-current-display-table) ; #<marker at 3963 in disp-table.el.gz>
  ("f"  . nvp-dev-features)
  ("Fr" . nvp-font-fontify-region-face)
  ("Fl" . nvp-font-list)
  ("h"  . nvp-dev-describe-hash)
  ("H"  . nvp-hook-add-or-remove)
  ("ld" . list-dynamic-libraries)
  ("lf" . list-faces-display)
  ("lF" . list-fontsets)
  ("li" . list-command-history)
  ("lc" . list-coding-systems)
  ("ls" . list-load-path-shadows)
  ("lt" . list-timers)
  ("lT" . list-threads)
  ("m"  . nvp-dev-describe-mode)
  ("o"  . nvp-dev-list-overlays)
  ("s"  . nvp-syntax-at-point)
  ("S"  . smie-config-show-indent)
  ("u"  . nvp-dev-stats-uniq)
  ("v"  . nvp-dev-describe-variable))


;; -------------------------------------------------------------------
;;; Commands 

;;;###autoload
(defun nvp-dev-advice-remove-all (sym)
  "Remove all advice from SYM."
  (interactive "aFunction: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

;; ;;;###autoload
;; (defun nvp-dev-make-and-reload ()
;;   "Make and reload package autoloads."
;;   (interactive)
;;   (call-process "make" nil 0 nil "-k")
;;   (let ((file (car (directory-files (expand-file-name ".") t "autoloads.el"))))
;;     (load-file file)))


;; -------------------------------------------------------------------
;;; Pretty print data

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

;; https://github.com/abo-abo/oremacs
(defun nvp-dev-describe-hash (variable)
  "Display the full documentation of VARIABLE (a symbol)."
  (interactive (let ((v (variable-at-point))
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
  (nvp-with-results-buffer (help-buffer)
    :title (format "Hash: %S" variable)
    (nvp-pp-hash variable)))

(defun nvp-dev-describe-variable (variable)
  "Try to pretty print VARIABLE in temp buffer."
  (interactive (list (nvp-tap 'evari)))
  (let ((val (if (symbolp variable) (eval variable)
               variable))
        (print-escape-newlines t)
        (print-circle t)
        ;; (print-gensym t)
        (inhibit-read-only t))
    (nvp-with-results-buffer (help-buffer)
      :title (format "Variable ('%s'): %S" (type-of val) variable)
      (princ (nvp-pp-variable-to-string val))
      (emacs-lisp-mode))))

(defun nvp-dev-describe-mode (&optional mode)
  (interactive (list (nvp-prefix 4 (nvp-read-mode) major-mode)))
  (let ((print-escape-newlines t)
        (print-circle t)
        (inhibit-read-only t)
        (funcs
         (--map (cons (symbol-name it) (symbol-value it))
                '(beginning-of-defun-function
                  end-of-defun-function
                  nvp-compile-function
                  nvp-mark-defun-function
                  nvp-check-buffer-function
                  nvp-help-at-point-functions
                  nvp-disassemble-function
                  nvp-test-function)))
        (vars
         (--map (cons (symbol-name it) (symbol-value it))
                '(nvp-abbrev-local-file
                  nvp-abbrev-local-table
                  nvp-mode-header-regex
                  nvp-mode-snippet-dir
                  nvp-mode-install-targets
                  completion-at-point-functions
                  company-backends
                  hippie-expand-try-functions-list)))
        (keys
         (--map (cons it (lookup-key (current-active-maps) (kbd it)))
                '("RET" "<f5>" "M-?"
                  "<f2>mzz" "C-c C-z"
                  "<f2>mc" "<f2>mt" "<f2>mT" "<f2>md" "<f2>mD")))
        (fonts (assoc mode nvp-mode-font-additions))
        (print-fn
         (lambda (lst &optional values)
           (dolist (i lst)
             (if (not values)
                 (princ (format ";; %s => %S\n" (car i) (cdr i)))
               (princ (format ";; %s" (car i)))
               (cl-prettyprint (cdr i))
               (princ "\n"))))))
    (nvp-with-results-buffer (help-buffer)
      :title (format "%S variables" mode)
      (princ ";;; Variables\n")
      (funcall print-fn vars 'values)
      (princ ";;; Functions\n")
      (funcall print-fn funcs)
      (princ ";;; Keys\n")
      (funcall print-fn keys)
      (when fonts
        (princ "\n;;; Fonts\n")
        (cl-prettyprint fonts)
        (princ "\n"))
      (emacs-lisp-mode))))

;; list my packages that have loaded
(defun nvp-dev-features (prefix)
  (interactive (list (nvp-prefix 4 (read-string "Prefix: ") "nvp")))
  (nvp-with-results-buffer (help-buffer)
    :title (format "Loaded '%s' features" prefix)
    (cl-prettyprint
     (sort 
      (--filter (string-prefix-p prefix (symbol-name it)) features)
      #'string-lessp))))


;; -------------------------------------------------------------------
;;; Overlays

;; https://www.emacswiki.org/emacs/EmacsOverlays
(defun nvp-dev-list-overlays (&optional pos)
  "Describe overlays at POS (default point)."
  (interactive)
  (setq pos (or pos (point)))
  (let ((overlays (overlays-at pos))
        (obuf (current-buffer))
        (props '(priority window category face mouse-face display
                          help-echo modification-hooks insert-in-front-hooks
                          insert-behind-hooks invisible intangible
                          isearch-open-invisible isearch-open-invisible-temporary
                          before-string after-string evaporate local-map keymap
                          field))
        start end text)
    (if (not overlays) (message "Nothing here :(")
      (nvp-with-results-buffer (help-buffer) :font-lock t
        :title (format "Overlays at %d in %S" pos obuf)
        (dolist (o overlays)
          (setq start (overlay-start o)
                end (overlay-end o)
                text (with-current-buffer obuf
                       (nvp-s 'bs start end)))
          (when (> (- end start) 13)
            (setq text (concat (substring text 0 10) "...")))
          (insert (propertize (format "From %d to %d: \"%s\":\n" start end text)
                              'face nil 'font-lock-face 'compilation-info))
          (dolist (prop props)
            (when (overlay-get o prop)
              (insert (propertize (format " %15S:" prop) 'face nil
                                  'font-lock-face font-lock-constant-face))
              (insert (format " %S\n" (overlay-get o prop))))))))))


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
        (nvp-results-title "Syntax at <marker>")
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
;;; Fonts

(defun nvp-font-fontify-region-face (face &optional beg end)
  "Fontify region or `thing-at-point' with font FACE.
With \\[universal-argument] prompt for THING at point."
  (interactive
   (let* ((thing
           (if current-prefix-arg
               (intern (read-from-minibuffer "Thing to fontify: "))
             'symbol))
          (bnds (nvp-tap 'btap thing)))
     (list (read-face-name "Fontifaction face: ") (car bnds) (cdr bnds))))
  (put-text-property beg end 'font-lock-face face))

;; https://gist.github.com/haxney/3055728
;; non-nil if monospaced font
(defun nvp-font-is-mono-p (font-family)
  (let (m-width l-width)
   (with-temp-buffer
     (set-window-buffer (selected-window) (current-buffer))
     (text-scale-set 4)
     (insert (propertize "l l l l l" 'face `((:family ,font-family))))
     (goto-char (line-end-position))
     (setq l-width (car (posn-x-y (posn-at-point))))
     (newline)
     (forward-line)
     (insert (propertize "m m m m m" 'face `((:family ,font-family) italic)))
     (goto-char (line-end-position))
     (setq m-width (car (posn-x-y (posn-at-point))))
     (eq l-width m-width))))

;; https://www.emacswiki.org/emacs/GoodFonts
(defun nvp-font-list ()
  "Display various available fonts."
  (interactive)
  (let ((str "The quick brown fox jumps over the lazy dog \
´`''\"\"1lI|¦!Ø0Oo{[()]}.,:; ")
        (font-families
         (cl-remove-duplicates 
          (sort (font-family-list) #'(lambda (x y) (string< (upcase x) (upcase y))))
          :test 'string=)))
    (nvp-with-results-buffer (help-buffer)
      (font-lock-mode)
      (dolist (ff (cl-remove-if-not 'nvp-font-is-mono-p font-families))
        (insert (propertize str 'font-lock-face `(:family ,ff)) ff "\n"
                (propertize str 'font-lock-face
                            `(:family ,ff :slant italic)) ff "\n")))))


;; -------------------------------------------------------------------
;;; Assorted

(defun nvp-dev-stats-uniq (beg end &optional _arg)
  "Print counts (case-insensitive) of unique words in region BEG to END."
  (interactive "r\nP")
  (require 'nvp-cache)
  (let ((ht (make-hash-table :test 'case-fold))
        (words (s-split-words (buffer-substring-no-properties beg end)))
        lst)
    (mapc (lambda (w) (cl-callf 1+ (gethash w ht 0))) words)
    (maphash (lambda (key val) (push (cons val key) lst)) ht)
    (setq lst (cl-sort lst #'> :key #'car))
    (nvp-with-results-buffer (help-buffer)
      :title "Word Counts"
      (pcase-dolist (`(,k . ,v) lst)
        (princ (format "%d: %s\n" k v))))))

(provide 'nvp-dev)
;;; nvp-dev.el ends here
