;;; nvp-trace.el --- trace -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'trace)
(require 'transient)

(nvp:decls :f (flatten-tree) :v (tracing-minor-mode))

(autoload 'find-library-name "find-func")
(autoload 'nvp-elisp-matching-forms "nvp-elisp" )
(autoload 's-split "s")
(autoload 'trace-tree-disable "trace-tree")


(defun nvp-trace-buffer (&optional and-go create)
  (let ((buf (if create
                 (get-buffer-create trace-buffer)
               (get-buffer trace-buffer))))
    (or buf (user-error "No trace-buffer"))
    (with-current-buffer buf
      (when (eq major-mode 'fundamental-mode)
        (trace-output-mode))
      (if and-go
          (pop-to-buffer (current-buffer))
        (display-buffer (current-buffer))))))

(defun nvp-trace-active-p ()
  (and (boundp 'trace-buffer)
       (buffer-live-p (get-buffer trace-buffer))))


;;; Menu

(transient-define-infix nvp-trace--toggle-inhibit nil
  :class 'transient-lisp-variable
  :variable 'inhibit-trace
  :reader (lambda (&rest _) (not inhibit-trace))
  :set-value (lambda (sym val)
               (set sym val)
               (tracing-update-mode-line)))

;;;###autoload(autoload 'nvp-trace-menu "nvp-trace" nil t)
(transient-define-prefix nvp-trace-menu ()
  "Trace"
  [["Trace"
    ("f" "Defun" trace-function)
    ("b" "Defun background" trace-function-background)
    ("u" "Untrace defun" untrace-function)
    ("q" "Untrace all" untrace-all)
    "--"
    ("i" "Inhibit trace" nvp-trace--toggle-inhibit)]
   ["Groups"
    ("g" "Group" nvp-trace-group)
    ("G" "Untrace group" nvp-untrace-group :if-non-nil tracing-minor-mode)
    ("l" "Library" nvp-trace-library)
    ("h" "Hooks" nvp-trace-hooks)]
   ["Results" :if nvp-trace-active-p
    ("j" "Display" nvp-trace-display-results)
    ("k" "Clear" nvp-trace-clear)]
   ["Tree mode" :if (lambda () (featurep 'trace-tree))
    ("tt" "Enable" trace-tree-enable)
    ("tq" "Disable" trace-tree-disable)]])


;; -------------------------------------------------------------------
;;; Tracing Minor Mode

(defvar tracing-mode-line-prefix " Tr")
(defvar tracing--mode-line-string tracing-mode-line-prefix)
(defvar tracing--current nil "Active trace names")
(defvar tracing--batch nil "Non-nil when doing batch action.")

(defun tracing-mode-line-toggle-inhibit (event)
  "Toggle `inhibit-trace' from the mode-line."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (setq inhibit-trace (not inhibit-trace))
    (force-mode-line-update t)))

(defun tracing--mode-line ()
  "Report current trace in the modeline."
  (let* ((face)
         (suffix (if inhibit-trace
                     (progn (setq face 'compilation-error) "off")
                   (let ((cnt (length tracing--current)))
                     (when (> cnt 0)
                       (setq face '(bold compilation-mode-line-exit)))
                     (number-to-string cnt)))))
    (concat
     tracing-mode-line-prefix ":"
     (apply #'propertize suffix
            ;; Fix(09/16/24): minor mode help-echo/local-map make this
            ;; impossible to click, at least when using powerline
            'help-echo (apply #'format "%s\nmouse-1: %s tracing"
                              (if inhibit-trace
                                  (list "Tracing is inhibited" "Enable")
                                (list "Tracing is active" "Inhibit")))
            'local-map (make-mode-line-mouse-map
                        'mouse-1 #'tracing-mode-line-toggle-inhibit)
            (and face (list 'face face))))))

(defun tracing-update-mode-line ()
  "Update active trace mode-line."
  (setq tracing--mode-line-string (tracing--mode-line))
  (when tracing-minor-mode
    (force-mode-line-update t)))

(defun nvp-trace-add (funcs &optional _type remove)
  "Track FUNCS and maybe enable/disable `tracing-minor-mode'.
If REMOVE is non-nil, remove FUNCS from tracking."
  (setq tracing--current
        (if remove
            (seq-remove (lambda (e) (memq e funcs)) tracing--current)
          (seq-uniq (append funcs tracing--current))))
  (cond ((zerop (length tracing--current))
         (tracing-minor-mode -1))
        (tracing-minor-mode
         (tracing-update-mode-line))
        (t (tracing-minor-mode 1))))

(defun nvp-trace-clear ()
  "Clear trace results buffer."
  (interactive)
  (when-let ((buf (get-buffer trace-buffer)))
    (and (buffer-live-p buf)
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (erase-buffer))))))

(defun tracing-list-untrace ()
  "Untrace function in list."
  (interactive)
  (when-let ((args (get-text-property (point) 'trace-args)))
    (apply #'untrace-function args)
    (revert-buffer nil t t)))

(defvar-keymap trace-active-list-keymap
  :doc "Keymap on links in list of traced functions."
  :parent button-map
  "u" #'tracing-list-untrace)

(defun nvp-trace-list ()
  "List functions currently traced."
  (interactive nil tracing-minor-mode)
  (unless tracing-minor-mode
    (user-error "No active trace"))
  (help-setup-xref (list #'nvp-trace-list)
		   (called-interactively-p 'interactive))
  (with-help-window (get-buffer-create "*Traced Functions*")
    (insert (propertize (format "%s" "Traced functions") 'face 'outline-1))
    (insert "\n\n")
    (dolist (fn tracing--current)
      (insert-text-button
       (symbol-name fn)
       'face 'button
       'keymap trace-active-list-keymap
       'trace-args (list fn)
       'action (lambda (_) (describe-function fn))
       'follow-link t
       'help-echo "mouse-1, RET: describe function")
      (insert "\n"))))

(defvar-keymap tracing-minor-mode-map
  "<f2> D j" #'nvp-trace-display-results
  "<f2> D q" #'untrace-all
  "<f2> D l" #'nvp-trace-list)

;;;###autoload
(define-minor-mode tracing-minor-mode
  "Minor mode active during tracing."
  :lighter (:eval tracing--mode-line-string)
  :keymap tracing-minor-mode-map
  :global t
  :interactive nil
  :group 'trace
  (if tracing-minor-mode
      (tracing-update-mode-line)
    (setq tracing--current nil)))


;;; Advices

;;;###autoload
(defun trace-function-internal@tracing-add (func &rest _)
  (or tracing--batch (nvp-trace-add (list func))))

;;;###autoload(advice-add 'trace-function-internal :after #'trace-function-internal@tracing-add '((name . "tracing-add")))

(define-advice untrace-function (:after (func) "tracing-remove")
  (or tracing--batch (nvp-trace-add (list func) nil t)))

(define-advice untrace-all (:around (orig) "tracing-remove-all")
  (let ((tracing--batch t))
    (funcall orig)
    (tracing-minor-mode -1)))


;;; Groups

(defvar nvp-trace-defun-forms
  '((defun cl-defun declare-function autoload cl-defmethod t)
    (defmacro cl-defmacro)
    (defsubst cl-defsubst)))

(defvar nvp-trace-group-alist nil)

(defun nvp-trace--read-groups (prompt &optional force-read untrace)
  "Load trace data and PROMPT for group to trace."
  (unless (bound-and-true-p nvp-trace-group-alist)
    (load-file (expand-file-name "trace-data.el" nvp/data)))
  (or (and (null force-read)
           (--when-let (completing-read-multiple prompt nvp-trace-group-alist)
             (cl-loop for g in it
                      collect (assq (intern g) nvp-trace-group-alist))))
      (let ((name (or untrace (read-string "Group name: ")))
            (funcs (read--expression prompt)))
        (and name (push (intern-soft name) funcs))
        (prog1 funcs
          (unless untrace
            (push funcs nvp-trace-group-alist))))))

;;;###autoload
(defun nvp-trace-group (groups &optional foreground)
  "Trace GROUPS of functions defined in `nvp-trace-group-alist'."
  (interactive (list (nvp-trace--read-groups "Trace: " current-prefix-arg)
                     current-prefix-arg))
  (let ((funcs (if (listp (car groups))
                   (apply #'append (--map (cdr it) groups))
                 (cdr groups)))
        (trace-fn (if foreground
                      #'trace-function-foreground
                    #'trace-function-background))
        (tracing--batch t))
    (dolist (fn funcs)
      (funcall trace-fn fn))
    (nvp-trace-add funcs)))

(defun nvp-untrace-group (funcs)
  "Untrace functions in GROUP."
  (interactive
   (list (apply #'append
                (--map
                 (cdr it)
                 (nvp-trace--read-groups "Untrace: " current-prefix-arg t)))))
  (let ((tracing--batch t))
    (dolist (fn funcs)
      (untrace-function fn))
    (nvp-trace-add funcs nil t)))

;;;###autoload
(defun nvp-trace-display-results (&optional and-go)
  "Show the trace results and maybe enable `trace-output-mode'."
  (interactive "P")
  (nvp-trace-buffer and-go))

;;;###autoload
(defun nvp-trace-library (library &optional macros filter)
  "Trace all top-level defun-like forms in library.
With \\[universal-argument], trace macros and substs as well.
With \\[universal-argument] \\[universal-argument] prompt for filter."
  (interactive (list (read-library-name) current-prefix-arg
                     (nvp:prefix '>=16 (read-string "Filter: "))))
  (require 'nvp-elisp)                  ;gather all defun-like forms
  (let* ((def-forms (if macros (flatten-tree nvp-trace-defun-forms)
                      (assoc 'defun nvp-trace-defun-forms)))
         (forms
          (with-temp-buffer
            (insert-file-contents (find-library-name library))
            (with-syntax-table emacs-lisp-mode-syntax-table
              (nvp-elisp-matching-forms def-forms))))
         (tracing--batch t))
    (when filter
      (setq forms (--filter (string-match-p filter (symbol-name it)) forms)))
    (dolist (fn forms)
      (trace-function-background fn))
    (nvp-trace-add forms 'library)
    (message "tracing %d funs from %s: %S" (length forms) library forms)))

;;;###autoload
(defun nvp-trace-hooks (&optional library)
  "Trace all hooks defined with `add-hook' in LIBRARY."
  (interactive
   (list (if current-prefix-arg (read-library-name) "nvp-mode-hooks")))
  (require 'nvp-elisp)
  (unless library (setq library "nvp-mode-hooks"))
  (cl-flet ((hook-p
              (form)
              (and (eq (car form) 'add-hook)
                   (eval (nth 2 form)))))
    (let ((forms (with-temp-buffer
                   (insert-file-contents (find-library-name library))
                   (with-syntax-table emacs-lisp-mode-syntax-table
                     (nvp-elisp-matching-forms #'hook-p))))
          (tracing--batch t))
      (dolist (fn forms)
        (trace-function-background fn))
      (nvp-trace-add forms 'hooks)
      (message "tracing hooks: %S" forms))))


;; -------------------------------------------------------------------
;;; Trace Output Mode

;; Skip over oen trace level. ARG is +1 or -1 for direction
(defun trace-output--forward-sexp-1 (arg)
  (let* ((cur-arrow "->")
         (level
          (progn (beginning-of-line)
                 (and (looking-at "\\(\\(?:| \\)*[0-9]+\\) \\([<>-]+\\)")
                      (prog1 (match-string 1)
                        (setq cur-arrow (match-string 2))))))
         (end-re (format "^%s %s" level (if (string= cur-arrow "<-") "->" "<-"))))
    (when level
      (if (or (and (string= "->" cur-arrow) (< arg 0))
              (and (string= "<-" cur-arrow) (> arg 0)))
          (zerop (forward-line arg))
        (catch 'term
          (progn (while (not (looking-at-p end-re))
                   (unless (zerop (forward-line arg))
                     (throw 'term nil)))
                 t))))))

;; Necessary for hideshow to hide levels properly
(defun trace-output-forward-sexp (&optional arg)
  "Function for `forward-sexp-function'.
ARG comes from `forward-sexp', which see."
  (or arg (setq arg 1))
  (let ((cnt (abs arg))
        (sign (if (> arg 0) 1 -1)))
    (catch 'term
      (while (> cnt 0)
        (beginning-of-line)
        (when (looking-at-p (regexp-quote trace-separator))
          (or (zerop (forward-line sign))
              (throw 'term nil)))
        (trace-output--forward-sexp-1 sign)
        (throw 'term nil)
        (cl-decf cnt)))
    cnt))

(defvar hs-special-modes-alist)
(with-eval-after-load 'hideshow
  (unless (assoc 'trace-output-mode hs-special-modes-alist)
    (push '(trace-output-mode
            "^\\(?:| \\)*[0-9]+ ->"
            "^\\(?:| \\)*[0-9]+ <-")
          hs-special-modes-alist)))

(defun trace-output-hl-line-range ()
  "Return the bounds of the call at point for `hl-line-range-function'."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^\\(?:| \\)*[0-9]+ \\(->\\|<-\\)")
        (let ((forward-p (equal "->" (match-string 1))))
          (cons (if forward-p (point) (line-beginning-position 2))
                (progn
                  (funcall (if forward-p #'forward-sexp #'backward-sexp))
                  (if forward-p (line-beginning-position 2) (point)))))
      (cons (point) (line-beginning-position 2)))))

(defun trace-output-untrace ()
  "Untrace traced function on current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\(?:| \\)*[0-9]+ \\(?:->\\|<-\\) (?\\([^ ]+\\)")
      (let ((fn (intern (match-string 1))))
        (when (and fn (y-or-n-p (format "Untrace %S? " fn)))
          (message "untracing %S" fn)
          (untrace-function fn)
          (setq tracing--current (delq fn tracing--current)))))))

(defvar trace-output-mode-keywords
  `((,(concat "^" (string-chop-newline trace-separator))
     . font-lock-comment-face)
    (,(rx bow (or "nil" "t") eow) . font-lock-constant-face)
    ("^\\(\\(?:| \\)+\\)?\\([0-9]+\\) \\(->\\) (\\([^ ]+\\)"
     (1 font-lock-comment-face nil t)
     (2 '(:inherit font-lock-number-face :weight bold))
     (3 '(:inherit font-lock-keyword-face :weight bold))
     (4 font-lock-function-name-face))
    ("^\\(\\(?:| \\)+\\)?\\([0-9]+\\) \\(<-\\) \\([^:]+\\)\\(:\\) \\([^\n]+\\)"
     (1 font-lock-comment-face nil t)
     (2 'font-lock-number-face)
     (3 'font-lock-operator-face)
     (4 'font-lock-property-use-face)
     (5 'font-lock-escape-face)
     (6 'font-lock-string-face)))
  "Font-locking for `trace-output-mode'.")

(defvar-keymap trace-output-mode-map
  :parent special-mode-map
  "j" #'next-line
  "k" #'previous-line
  "h" #'backward-char
  "l" #'forward-char
  "u" #'trace-output-untrace
  "f" #'forward-sexp
  "b" #'backward-sexp
  "n" #'forward-paragraph
  "p" #'backward-paragraph)

;;;###autoload
(define-derived-mode trace-output-mode special-mode "Trace"
  "Major mode for displaying trace results.

\\{trace-output-mode-map}"
  :abbrev-table nil
  :syntax-table emacs-lisp-mode-syntax-table
  (setq buffer-read-only nil)
  (let ((sep (regexp-quote (string-chop-newline trace-separator))))
    ;; Variables to set for hideshow to work
    (setq-local paragraph-separate sep)
    (setq-local comment-start sep))
  (setq-local comment-end "")
  (setq-local hs-hide-comments-when-hiding-all nil)
  (setq-local forward-sexp-function #'trace-output-forward-sexp)
  (setq-local font-lock-defaults (list trace-output-mode-keywords))
  (setq-local hl-line-range-function #'trace-output-hl-line-range)
  (hl-line-mode))

(provide 'nvp-trace)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-trace.el ends here
