;;; nvp-trace.el --- trace -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'trace)
(require 'transient)

(nvp:decls :f (flatten-tree))

(autoload 'find-library-name "find-func")
(autoload 'nvp-elisp-matching-forms "nvp-elisp" )
(autoload 's-split "s")
(autoload 'trace-tree-disable "trace-tree")


(defun nvp-trace-active-p ()
  (and (boundp 'trace-buffer)
       (buffer-live-p (get-buffer trace-buffer))))

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


;;; Menu

(nvp:transient-toggle nvp-trace-menu inhibit-trace)

;;;###autoload(autoload 'nvp-trace-menu "nvp-trace" nil t)
(transient-define-prefix nvp-trace-menu ()
  "Trace"
  [["Trace"
    ("f" "Defun" trace-function)
    ("b" "Defun background" trace-function-background)
    ("u" "Untrace defun" untrace-function)
    ("q" "Untrace all" nvp-untrace-all)
    ("Q" "Quit" nvp-trace-kill :if-non-nil nvp-tracing-minor-mode)
    "--"
    ("i" "Inhibit trace" nvp-trace-menu--toggle-inhibit-trace)]
   ["Groups"
    ("g" "Group" nvp-trace-group)
    ("G" "Untrace group" nvp-untrace-group :if-non-nil nvp-tracing-minor-mode)
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

(defvar nvp-tracing--mode-line " Trace")
(defvar nvp-trace--active nil "Non-nil when tracing")
(defvar nvp-trace--current nil "Active trace names")
(defvar nvp-trace--type nil "Active trace type")

(defun nvp-trace--abbreviate (names)
  (cond ((stringp names) names)
        (t (concat (nvp:as-string (car names)) "..."))))
;; ((<= (length names) 1) (car names))
;; (t (mapconcat
;;     (lambda (name)
;;       (mapconcat
;;        (lambda (ss) (substring ss 0 1))
;;        (s-split "-" (symbol-name name) t)
;;        "-"))
;;     names "|"))

(defun nvp-tracing-mode-line ()
  "Report current trace in the modeline."
  (if (null nvp-trace--current) " Trace"
    (format " Trace:%s:%d"
            (cond (inhibit-trace
                   (propertize "inhibited" 'face 'font-lock-warning-face))
                  ((length> nvp-trace--current 0) "run")
                  (t "inactive"))
            (if nvp-trace--current
                (length nvp-trace--current)
              0))))
;; (if (eq nvp-trace--type 'group)
;;     (nvp-trace--abbreviate nvp-trace--current)
;;   nvp-trace--current)

(defun nvp-trace-update-mode-line ()
  "Update tracing mode-line."
  (setq nvp-tracing--mode-line (nvp-tracing-mode-line))
  (force-mode-line-update))

(defun nvp-trace-add (funcs &optional type)
  (setq nvp-trace--current (seq-uniq (append funcs nvp-trace--current)))
  (setq nvp-trace--type type)           ; huh?
  (force-mode-line-update))

(defvar-keymap nvp-tracing-minor-mode-map
  "<f2> D j"   #'nvp-trace-display-results
  "<f2> D q"   #'nvp-untrace-all
  "<f2> D Q"   #'nvp-trace-kill)

(define-minor-mode nvp-tracing-minor-mode
  "Tracing minor mode."
  :lighter nvp-tracing--mode-line
  :keymap nvp-tracing-minor-mode-map
  :global t
  :group 'trace
  (when nvp-tracing-minor-mode
    (setq nvp-trace--current nil
          nvp-trace--type nil)
    (nvp-trace-update-mode-line)))

(defun nvp-trace-clear ()
  "Clear tracing buffer."
  (interactive)
  (when-let ((buf (get-buffer trace-buffer)))
    (and (buffer-live-p buf)
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (erase-buffer))))))

(defun nvp-trace-kill ()
  "Untrace everything and quit `nvp-tracing-minor-mode'."
  (interactive)
  (call-interactively #'untrace-all)
  (nvp-tracing-minor-mode -1))

(defun nvp-untrace-all ()
  "Untrace all functions and turn off minor mode."
  (interactive)
  (call-interactively #'untrace-all)
  (setq nvp-trace--current nil)
  (nvp-trace-update-mode-line))

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
  (or (listp (car groups)) (setq groups (list groups)))
  (--each groups
    (nvp-trace-add (cdr it) 'group)
    (--each it
      (funcall (if foreground
                   #'trace-function-foreground
                 #'trace-function-background)
               it)))
  (nvp-tracing-minor-mode 1))

(defun nvp-untrace-group (funcs)
  "Untrace functions in GROUP."
  (interactive
   (list (apply #'append
                (--map
                 (cdr it)
                 (nvp-trace--read-groups "Untrace: " current-prefix-arg t)))))
  (dolist (fn funcs)
    (untrace-function fn)
    (setq nvp-trace--current (delq fn nvp-trace--current)))
  (nvp-trace-update-mode-line))

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
              (nvp-elisp-matching-forms def-forms)))))
    (when filter
      (setq forms (--filter (string-match-p filter (symbol-name it)) forms)))
    (dolist (fn forms)
      (trace-function-background fn))
    (nvp-trace-add forms 'library)
    (message "tracing %d funs from %s: %S" (length forms) library forms))
  (nvp-tracing-minor-mode 1))

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
                     (nvp-elisp-matching-forms #'hook-p)))))
      (dolist (fn forms)
        (trace-function-background fn))
      (nvp-trace-add forms 'hooks)
      (message "tracing hooks: %S" forms)))
  (nvp-tracing-minor-mode 1))


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
          (setq nvp-trace--current (delq fn nvp-trace--current)))))))

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
