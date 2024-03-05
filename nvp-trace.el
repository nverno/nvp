;;; nvp-trace.el --- trace -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'transient)
(require 'trace)
(require 'trace-tree nil t)
(nvp:auto "find-func" 'read-library-name 'find-library-name)
(nvp:auto "nvp-elisp" 'nvp-elisp-matching-forms)
(nvp:auto "s" 's-split)
(nvp:auto "trace-tree" trace-tree-enable trace-tree-disable)
(nvp:decl flatten-tree)


;;;###autoload(autoload 'nvp-trace-menu "nvp-trace" nil t)
(transient-define-prefix nvp-trace-menu ()
  "Trace"
  [["Trace"
    ("f" "Defun" trace-function)
    ("b" "Defun background" trace-function-background)
    ("u" "Untrace defun" untrace-function)
    ("q" "Untrace all" nvp-untrace-all)
    ("Q" "Quit" nvp-trace-kill
     :if (lambda () (bound-and-true-p nvp-trace-minor-mode)))]
   ["Groups"
    ("g" "Group" nvp-trace-group)
    ("G" "Untrace group" nvp-untrace-group
     :if (lambda () (bound-and-true-p nvp-trace--current)))
    ("l" "Library" nvp-trace-library)
    ("h" "Hooks" nvp-trace-hooks)]]
  ["Results"
   ("j" "Jump to results" nvp-trace-display-results
    :if (lambda () (and (boundp 'trace-buffer)
                   (buffer-live-p (get-buffer trace-buffer)))))
   ("tt" "Enable trace-tree" trace-tree-enable
    :if (lambda () (featurep 'trace-tree)))
   ("tq" "Disable trace-tree" trace-tree-disable
    :if (lambda () (featurep 'trace-tree)))])

(defvar-local nvp-trace--mode-line " Trace")
(defvar nvp-trace--active nil "Non-nil when tracing")
(defvar nvp-trace--current nil "Active trace names")
(defvar nvp-trace--type nil "Active trace type")

(defun nvp-trace--abbreviate (names)
  (cond
   ((stringp names) names)
   (t (concat (nvp:as-string (car names)) "..."))
   ;; ((<= (length names) 1) (car names))
   ;; (t (mapconcat
   ;;     (lambda (name)
   ;;       (mapconcat
   ;;        (lambda (ss) (substring ss 0 1)) (s-split "-" (symbol-name name) t) "-"))
   ;;     names "|"))
   ))

(defun nvp-trace-mode-line ()
  "Report current trace in the modeline."
  (if (null nvp-trace--current) " Trace"
    (format " %sT[%s%s%s]"
            (if nvp-trace--active "*" "")
            (or nvp-trace--type "")
            (if nvp-trace--type ":" "")
            (if (eq nvp-trace--type 'grp)
                (nvp-trace--abbreviate nvp-trace--current)
              nvp-trace--current))))

(defun nvp-trace-update-mode-line ()
  "Update tracing mode-line."
  (setq nvp-trace--mode-line (nvp-trace-mode-line))
  (force-mode-line-update))

(defun nvp-trace-add (name &optional type)
  "Add NAME of TYPE to currently traced."
  (if (not nvp-trace--active)
      (setq nvp-trace--current name
            nvp-trace--type type)
    (pcase type
      ('grp (when (or (null nvp-trace--type)
                      (eq 'grp nvp-trace--type))
              (setq nvp-trace--current (append name (nvp:as-list nvp-trace--current)))))
      ;; XXX: library tracing just clobbers any traced groups
      ('lib (setq nvp-trace--current name
                  nvp-trace--type 'lib))
      (_)))
  (setq nvp-trace--active t)
  (nvp-trace-update-mode-line))

(defvar nvp-trace-minor-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "<f2> D j") #'nvp-trace-display-results)
    (define-key km (kbd "<f2> D q") #'nvp-untrace-all)
    (define-key km (kbd "<f2> D k") #'nvp-trace-kill)
    (easy-menu-define nil km nil
      '("Trace"
        ["Show output" nvp-trace-display-results t]
        ["Stop trace" nvp-untrace-all t]
        ["Quit" nvp-trace-kill t]))
    km))

(define-minor-mode nvp-trace-minor-mode "Tracing minor mode."
  :lighter nvp-trace--mode-line
  :keymap nvp-trace-minor-mode-map
  (if nvp-trace-minor-mode-map
      (nvp-trace-update-mode-line)
    (setq nvp-trace--current nil
          nvp-trace--active nil
          nvp-trace--type nil)))

(defun nvp-trace-kill ()
  (interactive)
  (call-interactively #'untrace-all)
  (nvp-trace-minor-mode -1))

(defun nvp-untrace-all ()
  "Untrace all functions and turn off minor mode."
  (interactive)
  (call-interactively #'untrace-all)
  (setq nvp-trace--active nil)
  (nvp-trace-update-mode-line))

(defvar nvp-trace-group-alist
  '((popups display-buffer
            pop-to-buffer
            pop-to-buffer-same-window
            switch-to-buffer-other-window
            switch-to-buffer-other-frame)))

(defvar nvp-trace-defun-forms
  '((defun cl-defun declare-function autoload cl-defmethod t)
    (defmacro cl-defmacro)
    (defsubst cl-defsubst)))

(eval-when-compile
  (defsubst nvp:trace--read (prompt &optional force-read)
    (or (and (null force-read)
             (--when-let (completing-read prompt nvp-trace-group-alist)
               (assq (intern it) nvp-trace-group-alist)))
        (let* ((name (read-string "Group name: "))
               (grp (cons (intern-soft name)
                          (read--expression prompt))))
          (prog1 grp
            (push grp nvp-trace-group-alist))))))

;;;###autoload
(defun nvp-trace-group (group)
  "Trace GROUP of functions defined in `nvp-trace-group-alist'."
  (interactive (list (nvp:trace--read "Trace: " current-prefix-arg)))
  (dolist (fn (cdr group))
    (trace-function-background fn))
  (nvp-trace-minor-mode 1)
  (nvp-trace-add (cdr group) 'grp))

(defun nvp-untrace-group (group)
  "Untrace functions in GROUP."
  (interactive (list (nvp:trace--read "Untrace: " current-prefix-arg)))
  (dolist (fn (cdr group))
    (untrace-function fn)
    (setq nvp-trace--current (delq fn nvp-trace--current)))
  (nvp-trace-update-mode-line))

;;;###autoload
(defun nvp-trace-display-results ()
  (interactive)
  (display-buffer trace-buffer))

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
    (message "tracing %d forms: %S" (length forms) forms))
  (nvp-trace-minor-mode 1)
  (nvp-trace-add library 'lib))

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
      (message "tracing: %S" forms)))
  (nvp-trace-minor-mode 1)
  (nvp-trace-add "hooks"))

(provide 'nvp-trace)
;;; nvp-trace.el ends here
