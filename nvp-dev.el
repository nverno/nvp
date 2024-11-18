;;; nvp-dev.el --- Elisp dev util -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; TODO:
;; - function to remove all methods from generic
;; - how to remove all notifications (filenotify) without storing them?
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'transient)
(require 'help-mode)
(nvp:decls :p (advice) :v (c-lang-constants pp-default-function))

(nvp:auto "nvp-util" 'nvp-s-wrap)
(nvp:auto "cl-extra" 'cl-prettyprint)

(define-button-type 'help-marker
  :supertype 'help-xref
  'help-function (lambda (m) (pop-to-buffer (marker-buffer m)) (goto-char m))
  'help-echo "mouse-2, RET: go to this marker")

;;;###autoload(autoload 'nvp-dev-menu "nvp-dev" nil t)
(transient-define-prefix nvp-dev-menu ()
  "Dev menu"
  [["At point"
    ("s" "Syntax" nvp-syntax-at-point)
    ("o" "Overlays" nvp-dev-list-overlays)]
   ["Dump"
    ("v" "Variable" nvp-dev-describe-variable)
    ("m" "Mode config" nvp-dev-describe-mode)]
   ["Modify"
    ("/t" "Transient" nvp-transient-menu :transient transient--do-replace)
    (":a" "Remove advice" nvp-dev-advice-remove-all)
    (":h" "Add/Remove hook" nvp-hook-add-or-remove)]]
  [["Resources"
    ("t" "Timers" list-timers)
    ("T" "Threads" list-threads)]
   ["Load"
    ("lf" "List nvp features" nvp-dev-features)
    ("ld" "List dynamic libs" list-dynamic-libraries)
    ("ls" "Load-path shadows" list-load-path-shadows)]])

(nvp:bindings nvp-dev-keymap nil
  :prefix "Dev"
  ("<f2>" . nvp-dev-menu)
  ("a"    . nvp-dev-advice-remove-all)
  ("D"    . describe-current-display-table) ; #<marker at 3963 in disp-table.el.gz>
  ("f"    . nvp-dev-features)
  ("H"    . nvp-hook-add-or-remove)
  ("ld"   . list-dynamic-libraries)
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

;;;###autoload
(defun nvp-dev-advice-remove-all (sym)
  "Remove all advice from SYM."
  (interactive
   ;; Completing read for advices copied from `advice-remove'
   (let* ((pred (lambda (sym) (advice--p (advice--symbol-function sym))))
          (default (when-let* ((f (function-called-at-point))
                               ((funcall pred f)))
                     (symbol-name f)))
          (prompt (format-prompt "Remove advices from function" default)))
     (list (intern (completing-read prompt obarray pred t nil nil default)))))
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))


;;; Pretty Printing datastructures

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
       (push (list (propertize (cl-prin1-to-string key) 'font-lock-face 'bold)
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
                         (insert (propertize "⬎\n" 'font-lock-face 'bold)))
                       (,fn ,var level)
                       (when (> level 0)
                         (remove-text-properties
                          (point-min) (point-max) '(display)))
                       (buffer-string))))
      (let ((str (pcase var
                   ((pred hash-table-p)
                    (get-string nvp-pp-hash var))
                   ((pred cl-struct-p)
                    (get-string nvp-pp-struct var))
                   (_ (nvp-pp--format-string (cl-prin1-to-string var))))))
        (if (> level 0)
            (replace-regexp-in-string
             "\n" (concat (make-string level ? ) "\n") str)
          str)))))

(defun nvp-dev--fontify-syntax ()
  "Fontify strings/comments w/o clobbering current text properties."
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (setq-local syntax-propertize-function #'elisp-mode-syntax-propertize)
  (font-lock-default-fontify-syntactically (point-min) (point-max)))

(defun nvp-dev-describe-variable (variable)
  "Pretty print VARIABLE."
  (interactive (list (nvp:tap 'evari)))
  (nvp:with-help-setup (nvp-dev-describe-variable variable))
  (let* ((val (if (symbolp variable) (eval variable)
                variable))
         (print-escape-newlines t)
         (print-circle t)
         (print-length nil)
         (inhibit-read-only t))
    (nvp:with-help-window
      :title (format "Variable (%s): `%S'" (type-of val) variable)
      :buffer (help-buffer)
      (insert (nvp-pp-variable-to-string val 0))
      (and (= (char-before) ?\n) (delete-char -1))
      (nvp-dev--fontify-syntax))))

;;;###autoload
(defun nvp-dev-describe-mode (&optional mode)
  (interactive (list (nvp:prefix 4 (nvp-read-mode) (or nvp-mode-name major-mode))))
  (nvp:with-help-setup (nvp-dev-describe-mode mode))
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
                     '(nvp-local-abbrev-file
                       nvp-local-abbrev-table
                       nvp-mode-header-regex
                       completion-at-point-functions
                       company-backends
                       hippie-expand-try-functions-list)))
        (keys (--map (cons it (lookup-key (current-active-maps) (kbd it)))
                     '("RET" "<f5>" "M-?"
                       "<f2>mm" "<f2>mc" "<f2>mt" "<f2>mT" "<f2>md" "<f2>mD")))
        (fonts (assoc mode nvp-mode-font-additions))
        (print-fn
         (lambda (lst &optional column-name)
           (cl--print-table
            (list (or column-name "Symbol") "Value")
            (mapcar (lambda (el)
                      (list (propertize (car el) 'font-lock-face 'bold)
                            (cl-prin1-to-string (cdr el))))
                    lst))
           (insert "\n"))))
    (cl-macrolet ((header (str)
                    (macroexp-let2 nil str str
                      `(insert (propertize ,str 'font-lock-face 'bold) "\n"
                               (make-string (length ,str) ?-) "\n"))))
      (nvp:with-help-window
        :title (format "%S variables" mode)
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

(defun nvp-dev-features (filter)
  "List loaded `features' matching \"^nvp\\b\" or FILTER."
  (interactive (list (nvp:prefix 4 (read-string "Regexp filter: ") "^nvp\\b")))
  (nvp:with-help-setup (nvp-dev-features filter)
    (let* ((fs (sort (--filter (string-match-p filter (symbol-name it))
                               features)
                     #'string-lessp))
           (title (format "Features filtered by /%s/ (count = %d)"
                          filter (length fs))))
      (nvp:with-help-window :title title
        (dolist (feat fs)
          (princ feat)
          (terpri))))))

;;;###autoload
(defun nvp-dev-c-lang-constants (mode)
  "Dump `c-lang-constants' for MODE."
  (interactive (list (intern (nvp-read-mode))))
  (require 'cc-mode)
  (or mode (setq mode major-mode))
  (nvp:with-help-setup (nvp-dev-c-lang-constants mode)
    (nvp:with-help-window
      :title (format "c-lang-constants for %S" mode)
      (let ((mode (intern (string-remove-suffix "-mode" (symbol-name mode))))
            (print-escape-newlines t)
            (print-circle t))
        (obarray-map
         (lambda (v)
           (when (symbolp v)
             (insert (propertize (format "%S" v)
                                 'font-lock-face 'font-lock-constant-face))
             (princ ": ")
             (condition-case nil
                 (eval `(prin1 (c-lang-const ,v ,mode)))
               (error (princ "<failed>")))
             (terpri)))
         c-lang-constants)))))

(defun nvp-dev-list-overlays (marker)
  "Describe overlays at MARKER (default point)."
  (interactive (list (point-marker)))
  (unless (and (markerp marker)
               (buffer-live-p (marker-buffer marker)))
    (user-error "Bad marker: %S" marker))
  ;; Note(10/08/24): Modified from
  ;; https://www.emacswiki.org/emacs/EmacsOverlays
  (with-current-buffer (marker-buffer marker)
    (let* ((pos (marker-position marker))
           (overlays (overlays-at pos))
           (obuf (current-buffer))
           (props '( priority window category face mouse-face display
                     help-echo modification-hooks insert-in-front-hooks
                     insert-behind-hooks invisible intangible
                     isearch-open-invisible isearch-open-invisible-temporary
                     before-string after-string evaporate local-map keymap
                     field))
           start end text)
      (if (not overlays)
          (message "Nothing here :(")
        (nvp:with-help-setup (nvp-dev-list-overlays marker)
          (nvp:with-help-window
            :title "Overlays at <marker>"
            :marker marker
            (dolist (o overlays)
              (setq start (overlay-start o)
                    end (overlay-end o)
                    text (with-current-buffer obuf
                           (buffer-substring-no-properties start end)))
              (when (> (- end start) 55)
                (setq text (concat (substring text 0 10) "...")))
              (insert (propertize (format "Overlay from %d to %d" start end)
                                  'face nil 'font-lock-face
                                  'font-lock-function-name-face)
                      ": \"" text "\"\n")
              (dolist (prop props)
                (when (overlay-get o prop)
                  (insert (propertize (format " %15S:" prop)
                                      'face nil
                                      'font-lock-face 'font-lock-constant-face))
                  (insert (format " %S\n" (overlay-get o prop)))))
              (insert "\n"))))))))


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
(defun nvp-syntax-at-point (marker)
  "Display info about syntax at point."
  (interactive (list (point-marker)))
  (unless (and (markerp marker)
               (buffer-live-p (marker-buffer marker)))
    (user-error "Bad marker: %S" marker))
  (nvp:with-help-setup (nvp-syntax-at-point marker)
    (with-current-buffer (marker-buffer marker)
      (let ((ppss (syntax-ppss marker))
            (help-str (nvp:lazy-val nvp-syntax-at-point-help)))
        (nvp:with-help-window
          :title "Syntax at <marker>"
          :marker marker
          (save-excursion (princ (apply #'format help-str ppss)))
          (setq-local comment-start "; "
                      fill-column 85
                      comment-column 30)
          (while (not (eobp))
            (when (looking-at-p comment-start)
              (insert "|"))
            (comment-indent)
            (forward-line 1))
          (hl-line-mode))))))

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

(provide 'nvp-dev)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-dev.el ends here
