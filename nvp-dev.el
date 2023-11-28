;;; nvp-dev.el --- elisp devel helpers -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; TODO:
;; - function to remove all methods from generic
;; - how to remove all notifications (filenotify) without storing them?
;; - useful smie debugging output: #<marker at 7444 in tuareg-opam.el>
;;
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-display))
(require 'transient)
(require 'help-mode)
(require 'nvp)

(nvp:decls :p (advice treesit) :f (nvp-read-mode)
           :v (c-lang-constants pp-default-function))
(nvp:auto "nvp-util" 'nvp-s-wrap)
(nvp:auto "cl-extra" 'cl-prettyprint)

(define-button-type 'help-marker
  :supertype 'help-xref
  'help-function (lambda (m) (pop-to-buffer (marker-buffer m)) (goto-char m))
  'help-echo (purecopy "mouse-2, RET: go to this marker"))

;;;###autoload(autoload 'nvp-dev-menu "nvp-dev")
(transient-define-prefix nvp-dev-menu ()
  "Dev menu"
  [["At point"
    ("s" "Syntax" nvp-syntax-at-point)
    ("o" "Overlays" nvp-dev-list-overlays)]
   ["Display"
    ("v" "Variable" nvp-dev-describe-variable)
    ("m" "Mode config" nvp-dev-describe-mode)
    ("c" "Charset" nvp-help-display-charset)
    ("C" "Coding systems" list-coding-systems)
    ("h" "Command history" list-command-history)]
   ["Modify"
    ("/t" "Transient" nvp-transient-menu :transient transient--do-replace)
    (":a" "Remove advice" nvp-dev-advice-remove-all)
    (":h" "Add/Remove hook" nvp-hook-add-or-remove)
    (":f" "Font face on region" nvp-font-fontify-region-face)]]
  [["Resources"
    ("t" "Timers" list-timers)
    ("T" "Threads" list-threads)]
   ["Fonts"
    ("ff" "Faces" list-faces-display)
    ("fF" "Fonts" nvp-font-list)
    ("fa" "Available fontsets" list-fontsets)]
   ["Load"
    ("lf" "List nvp features" nvp-dev-features)
    ("ld" "List dynamic libs" list-dynamic-libraries)
    ("ls" "Load-path shadows" list-load-path-shadows)]])

(nvp:bindings nvp-dev-keymap nil
  :create t
  ("<f2>" . nvp-dev-menu)
  ("a"    . nvp-dev-advice-remove-all)
  ("c"    . nvp-help-list-charsets)
  ("D"    . describe-current-display-table) ; #<marker at 3963 in disp-table.el.gz>
  ("f"    . nvp-dev-features)
  ("Fr"   . nvp-font-fontify-region-face)
  ("Fl"   . nvp-font-list)
  ("H"    . nvp-hook-add-or-remove)
  ("ld"   . list-dynamic-libraries)
  ("lf"   . list-faces-display)
  ("lF"   . list-fontsets)
  ("li"   . list-command-history)
  ("lc"   . list-coding-systems)
  ("ls"   . list-load-path-shadows)
  ("lt"   . list-timers)
  ("lT"   . list-threads)
  ("m"    . nvp-dev-describe-mode)
  ("o"    . nvp-dev-list-overlays)
  ("s"    . nvp-syntax-at-point)
  ("S"    . smie-config-show-indent)
  ("v"    . nvp-dev-describe-variable))

;; -------------------------------------------------------------------
;;; Advice

;;;###autoload
(defun nvp-dev-advice-remove-all (sym)
  "Remove all advice from SYM."
  (interactive "aFunction: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

;; -------------------------------------------------------------------
;;; Pretty print data

(defun nvp-pp-struct (struct &optional level)
  "Pretty print STRUCT entry."
  (let* ((type (type-of struct))
         (slots (cl--class-slots (cl-find-class type)))
         (slot-strings
          (mapcar
           (lambda (slot)
             (let ((name (cl--slot-descriptor-name slot)))
               (list (if (and level (> level 0))
                         (string-pad (cl-prin1-to-string name) 15)
                       (cl-prin1-to-string name))
                     (cl-prin1-to-string (cl--slot-descriptor-type slot))
                     (cl-prin1-to-string (cl--slot-descriptor-initform slot))
                     (nvp-pp-variable-to-string
                      (cl-struct-slot-value type name struct)
                      (and level (1+ level))))))
           slots)))
    (cl--print-table '("Name        " "Type" "Default" "Current") slot-strings)
    (insert "\n")))

;; ielm's nice formatting: #<marker at 14912 in ielm.el.gz>
(defun nvp-pp-hash (hash &optional level)
  (let (entries)
    (maphash
     (lambda (key val)
       (push (list (propertize (cl-prin1-to-string key) 'face 'bold)
                   (nvp-pp-variable-to-string val (and level (1+ level))))
             entries))
     hash)
    (cl--print-table '("Key" "Value") entries)))

(defun nvp-pp--format-string (print-rep)
  (if (< (length print-rep) 68)
      print-rep
    (with-temp-buffer
      (insert print-rep)
      (pp-buffer)
      (buffer-string))))

(defun nvp-pp-variable-to-string (variable &optional level)
  (or level (setq level 0))
  (let ((print-escape-newlines nil)
        (print-circle t)
        (print-quoted t)
        ;; (pp-default-function 'pp-29)
        (var (or (and (symbolp variable)
                      (boundp variable)
                      (symbol-value variable))
                 variable)))
    (cl-macrolet ((get-string (fn var)
                    `(with-temp-buffer
                       (when (> level 0)
                         (insert (propertize "⬎\n" 'face 'bold)))
                       (,fn ,var level)
                       (when (> level 0)
                         (remove-text-properties (point-min) (point-max) '(display)))
                       (buffer-string))))
      (let ((str (pcase var
                   ((pred hash-table-p)
                    (get-string nvp-pp-hash var))
                   ((pred cl-struct-p)
                    (get-string nvp-pp-struct var))
                   (_ (nvp-pp--format-string (cl-prin1-to-string var))))))
        (if (> level 0)
            (replace-regexp-in-string "\n" (concat (make-string level ? ) "\n") str)
          str)))))

(defun nvp-dev--fontify-syntax ()
  ;; Fontify strings/comments w/o clobbering current text properties
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (setq-local syntax-propertize-function #'elisp-mode-syntax-propertize)
  (font-lock-default-fontify-syntactically (point-min) (point-max)))

(defun nvp-dev-describe-variable (variable)
  "Pretty print VARIABLE."
  (interactive (list (nvp:tap 'evari)))
  (let* ((val (if (symbolp variable) (eval variable)
                variable))
         (print-escape-newlines t)
         (print-circle t)
         (print-length nil)
         ;; (print-gensym t)
         (inhibit-read-only t))
    (nvp:with-results-buffer
      :title (format "Variable ('%s'): %S" (type-of val) variable)
      :buffer (format "*describe[%s]*" variable)
      :revert-fn (lambda (&rest _) (funcall #'nvp-dev-describe-variable variable))
      :action :none
      (insert (nvp-pp-variable-to-string val 0))
      (and (= (char-before) ?\n) (delete-char -1))
      (nvp-dev--fontify-syntax))))

;;;###autoload
(defun nvp-dev-describe-mode (&optional mode)
  (interactive (list (nvp:prefix 4 (nvp-read-mode) (or nvp-mode-name major-mode))))
  (let ((print-escape-newlines t)
        (print-circle t)
        (inhibit-read-only t)
        (pp-default-function 'pp-29)
        (funcs (--map (cons (symbol-name it) (symbol-value it))
                      `(beginning-of-defun-function
                        end-of-defun-function
                        forward-sexp-function
                        forward-sentence-function
                        nvp-mark-defun-function
                        nvp-fill-paragraph-function
                        nvp-help-at-point-functions
                        ,@nvp-mode-default-functions)))
        (vars (--map (cons (symbol-name it) (symbol-value it))
                     '(nvp-abbrev-local-file
                       nvp-abbrev-local-table
                       nvp-mode-header-regex
                       completion-at-point-functions
                       company-backends
                       hippie-expand-try-functions-list)))
        (keys (--map (cons it (lookup-key (current-active-maps) (kbd it)))
                     '("RET" "<f5>" "M-?"
                       "<f2>mzz" "C-c C-z"
                       "<f2>mc" "<f2>mt" "<f2>mT" "<f2>md" "<f2>mD")))
        (fonts (assoc mode nvp-mode-font-additions))
        (print-fn
         (lambda (lst &optional column-name)
           (cl--print-table
            (list (or column-name "Symbol") "Value")
            (mapcar (lambda (el) (list (propertize (car el) 'face 'bold)
                                  (cl-prin1-to-string (cdr el))))
                    lst))
           (insert "\n"))))
    (cl-macrolet ((header (str)
                    (macroexp-let2 nil str str
                      `(insert (propertize ,str 'face 'bold) "\n"
                               (make-string (length ,str) ?-) "\n"))))
      (nvp:with-results-buffer :title (format "%S variables" mode)
        :action :none
        (header (symbol-name mode))
        (--when-let (gethash mode nvp-mode-cache) (nvp-pp-struct it))
        (funcall print-fn vars "Variables")
        (funcall print-fn funcs "Functions")
        (funcall print-fn keys "Keys")
        (when fonts
          (header "\nFonts")
          (ignore-errors (cl-prettyexpand fonts))
          (insert "\n"))
        (nvp-dev--fontify-syntax)
        (pop-to-buffer (current-buffer))))))

(defun nvp-dev-features (prefix)
  "List my loaded `features', or prompt for PREFIX."
  (interactive (list (nvp:prefix 4 (read-string "Prefix: ") "nvp")))
  (let* ((fs (sort
              (--filter (string-prefix-p prefix (symbol-name it)) features)
              #'string-lessp))
         (title (format "Loaded '%s' features (%d)" prefix (length fs))))
    (nvp:with-results-buffer :title title
      (cl-prettyprint fs))))

;; dump lang's `c-lang-constants'

;;;###autoload
(defun nvp-dev-c-lang-constants (&optional mode)
  (interactive (list (nvp-read-mode)))
  (require 'cc-mode)
  (setq mode (intern (string-remove-suffix "-mode" (or mode major-mode))))
  (let ((print-escape-newlines t)
        (print-circle t)
        (inhibit-read-only t))
    (nvp:with-results-buffer :title (format "c-lang-constants for %s" mode)
      (dolist (v (append c-lang-constants ()))
        (when (symbolp v)
          (princ v)
          (princ ": ")
          (condition-case nil
              (eval `(prin1 (c-lang-const ,v ,mode)))
            (error (princ "<failed>")))
          (terpri))))))

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
      (nvp:with-results-buffer :title (format "Overlays at %d in %S" pos obuf)
        (dolist (o overlays)
          (setq start (overlay-start o)
                end (overlay-end o)
                text (with-current-buffer obuf
                       (buffer-substring-no-properties start end)))
          (when (> (- end start) 13)
            (setq text (concat (substring text 0 10) "...")))
          (insert (propertize (format "From %d to %d: \"%s\":\n" start end text)
                              'face nil 'font-lock-face 'compilation-info))
          (dolist (prop props)
            (when (overlay-get o prop)
              (insert (propertize (format " %15S:" prop) 'face nil
                                  'font-lock-face font-lock-constant-face))
              (insert (format " %S\n" (overlay-get o prop))))))
        (font-lock-flush)
        (font-lock-ensure)))))


;; -------------------------------------------------------------------
;;; Syntax

(nvp:lazy-defvar nvp-syntax-at-point-help
  (lambda ()
    (let
        ((help-strs
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
            "11).... Possible further internal information used by \
‘parse-partial-sexp’.")))
      (mapconcat 'identity
                 (nconc
                  (cl-loop for i from 0 upto 10
                           collect
                           (concat (number-to-string i) ") %s "
                                   (nvp-s-wrap 45 (nth i help-strs) "; ")))
                  (list (nth 11 help-strs)))
                 "\n"))))

;;;###autoload
(defun nvp-syntax-at-point (marker &optional action)
  "Display info about syntax at point.
With prefix, display in same frame using `display-buffer' ACTION."
  (interactive (list (point-marker) (prefix-numeric-value current-prefix-arg)))
  (let ((ppss (syntax-ppss marker))
        (help-str (nvp:lazy-val nvp-syntax-at-point-help)))
    (help-setup-xref (list #'nvp-syntax-at-point marker)
                     (called-interactively-p 'interactive))
    (nvp:display-buffer-with-action action
      (with-help-window (help-buffer)
        (princ (nvp:centered-header "Syntax at <marker>"))
        (princ (apply #'format help-str ppss))
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
;;   (nvp:display-buffer-with-action 4
;;     (with-help-window (help-buffer)
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
;;; Fonts / Chars

(defun nvp-font-fontify-region-face (face &optional beg end)
  "Fontify region or `thing-at-point' with font FACE.
With \\[universal-argument] prompt for THING at point."
  (interactive
   (let* ((thing
           (if current-prefix-arg
               (intern (read-from-minibuffer "Thing to fontify: "))
             'symbol))
          (bnds (nvp:tap 'btap thing)))
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
    (nvp:with-results-buffer :buffer (help-buffer) :title "Available Fonts"
      (font-lock-mode)
      (dolist (ff (cl-remove-if-not 'nvp-font-is-mono-p font-families))
        (insert (propertize str 'font-lock-face `(:family ,ff)) ff "\n"
                (propertize str 'font-lock-face
                            `(:family ,ff :slant italic))
                ff "\n")))))

(provide 'nvp-dev)
;;; nvp-dev.el ends here
