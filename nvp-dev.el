;;; nvp-dev.el --- elisp devel helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; TODO:
;; - function to remove all methods from generic
;; - how to remove all notifications (filenotify) without storing them?

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(require 'nvp)
(require 'nvp-display)
(require 'help-mode)
(nvp-declare :pkg "nadvice" advice-mapc advice-remove)
(nvp-autoload "nvp-string" nvp-s-wrap nvp-s-center nvp-s-repeat)
(autoload 's-split-words "s")
(defvar nvp-mode-cache)

(define-button-type 'help-marker
  :supertype 'help-xref
  'help-function (lambda (m) (pop-to-buffer (marker-buffer m)) (goto-char m))
  'help-echo (purecopy "mouse-2, RET: go to this marker"))

;;;###autoload
(defun nvp-dev-load (&optional _arg)
  (interactive "P")
  (let* ((keys (this-command-keys-vector))
         (next-keys (append (listify-key-sequence keys) (list (read-event)))))
    (message "this-command-keys: %S, vector: %S, keys: %S, next-keys: %S"
             (this-command-keys) (this-command-keys-vector)
             keys next-keys)
    ;; (setq current-prefix-arg arg)
    ;; (execute-kbd-macro (vconcat next-keys))
    ))

;; HACK: undefine `nvp-dev-load' from keymap when file is loaded
;; should replace this with a better system
(defvar nvp-dev-keymap)
(define-key nvp-dev-keymap (nvp-input 'lce) nil)
(nvp-bind-keys nvp-dev-keymap
  ("a"  . nvp-dev-advice-remove-all)
  ("c"  . nvp-help-list-charsets)
  ("D"  . describe-current-display-table) ; #<marker at 3963 in disp-table.el.gz>
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
  ("m"  . nvp-dev-make-and-reload)
  ("o"  . nvp-dev-list-overlays)
  ("s"  . nvp-syntax-at-point)
  ("u"  . nvp-dev-stats-uniq)
  ("v"  . nvp-dev-describe-variable))

;; -------------------------------------------------------------------
;;; Utils

;; princ centered TITLE
(defsubst nvp-dev--princ-title (title &optional width char)
  (or width (setq width 85))
  (or char (setq char "~"))
  (princ (format "\n%s\n%s\n\n" (nvp-s-center width title)
                 (nvp-s-repeat width "~"))))


;; -------------------------------------------------------------------
;;; Commands 

(defun nvp-dev-advice-remove-all (sym)
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
    (nvp-dev--princ-title (format "Hash: %S" variable))
    (maphash (lambda (key value)
               (pp key)
               (princ " => ")
               (pp value)
               (terpri))
             (symbol-value variable))))

;; TODO: pretty printing `cl-defstruct'
;; cl-prettyprint: #<marker at 22951 in cl-extra.el.gz>
;;;###autoload
(defun nvp-dev-describe-variable (variable)
  "Try to pretty print VARIABLE in temp buffer."
  (interactive (list (nvp-tap 'evari)))
  (pcase variable
    ((pred hash-table-p)
     (nvp-dev-describe-hash variable))
    (_ (user-error "TOTO"))))


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
        (nvp-dev--princ-title (format "Overlays at %d in %S" pos (current-buffer)))
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
;;; Fonts

;;;###autoload
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

;; -------------------------------------------------------------------
;;; Display

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
;;;###autoload
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

;;;###autoload
(defun nvp-dev-stats-uniq (beg end &optional _arg)
  "Print counts (case-insensitive) of unique words in region BEG to END."
  (interactive "r\nP")
  (require 'nvp-hash)
  (let ((ht (make-hash-table :test 'case-fold))
        (words (s-split-words (buffer-substring-no-properties beg end)))
        lst)
    (mapc (lambda (w) (cl-callf + (gethash w ht 0) 1)) words)
    (maphash (lambda (key val) (push (cons val key) lst)) ht)
    (setq lst (cl-sort lst #'> :key #'car))
    (nvp-with-results-buffer (help-buffer)
      (nvp-dev--princ-title "Word Counts")
      (pcase-dolist (`(,k . ,v) lst)
        (princ (format "%d: %s\n" k v))))))

(provide 'nvp-dev)
;;; nvp-dev.el ends here
