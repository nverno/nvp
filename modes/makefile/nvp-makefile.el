;;; nvp-makefile.el --- make helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; make -q foo => exit 0 if foo is up-to-date

;; TODO:
;; Base:
;; - generic compile targets
;; - additional font-locking: shell/define/info/warn/error
;; Extra:
;; - debug?
;; - fold: directives, rules, comments
;; - align: similar to sh-mode rules => equals, EOL comments/backslashes
;; - indent: directives
;; - macrostep: update w/ dyn. table

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-font))
(require 'nvp)
(require 'make-mode)
(nvp-decls)

;;; Navigation

(defvar nvp-makefile-open/close
  (eval-when-compile
    (let ((openers (concat "^" (regexp-opt '("ifeq" "ifneq" "define"))))
          (closers (concat "^" (regexp-opt '("endif" "endef")))))
      `(,openers ,closers))))

(defvar nvp-makefile-defun-regexp
  (concat (car nvp-makefile-open/close) "\\|" "^[^# \t\n]+:"))

(defun nvp-makefile--match-opener (search-fn)
  (beginning-of-line)
  (let ((index (if (eq search-fn 're-search-backward) (cons 1 0)
                 (cons 0 1))))
    (if (looking-at (concat "^else\\|" (nth (car index) nvp-makefile-open/close)))
        (funcall search-fn (nth (cdr index) nvp-makefile-open/close) nil t))))

(defsubst nvp-makefile--defun-line-p ()
  (save-excursion
    (beginning-of-line 1)
    (looking-at-p nvp-makefile-defun-regexp)))

(defsubst nvp-makefile--skip-escapes (search-fn)
  (if (eq search-fn 're-search-backward)
      (eq (point-at-bol) (nvp-goto 'boll))
    (eq (point-at-eol) (nvp-goto 'eoll))))

(defun nvp-makefile--beginning-of-defun (arg)
  (let ((search-fn (if (> arg 0) #'re-search-backward #'re-search-forward))
        (pos (point-marker)))
    (and (< arg 0)                   ;searching forward -- skip initial beg . 
         (nvp-makefile--defun-line-p)
         (end-of-line))
    (while (and (funcall search-fn nvp-makefile-defun-regexp nil 'move)
                (not (nvp-makefile--skip-escapes search-fn))))
    (if (nvp-makefile--defun-line-p)
        (or (nvp-point 'boll) (point))  ;found beg
      (and (goto-char pos) nil))))     ;failed

(defun nvp-makefile-beginning-of-defun (&optional arg)
  "Treats target blocks as defuns."
  (when (or (null arg) (zerop arg)) (setq arg 1))
  (let (found)
    (while (and (not (zerop arg))
                (let ((searching-p (nvp-makefile--beginning-of-defun arg)))
                  (when (and searching-p (null found))
                    (setq found t))
                  searching-p))
      (setq arg (if (> arg 0) (1- arg) (1+ arg))))
    found))

(defun nvp-makefile-end-of-defun ()
  "Skips to end of tabbed block."
  (unless (nvp-makefile--match-opener 're-search-forward)
    (while (and (not (eobp))
                (nvp-goto 'bonll)
                (not (looking-at-p
                      (concat "^\\s-*$\\|" nvp-makefile-defun-regexp)))))
    (point)))

;;; Font-lock
;; TODO:
;; - remove string fontification in #define blocks where it is incorrect.

;; better to fontify using `forward-sexp' to allow for closing parens in command
(nvp-font-lock-add-defaults 'makefile-gmake-mode
  ("\\$(\\s-*info\\s-*\\([^)]*\\)" (1 'nvp-info-face prepend))
  ("\\$(\\s-*warning\\s-*\\([^)]*\\)" (1 'nvp-warning-face prepend))
  ("\\$(\\s-*error\\s-*\\([^)]*\\)" (1 'nvp-error-face prepend)))

;; `makefile-dependency-regex' => note this doesn't take into account quoting
;; `makefile-macroassign-regex' => doesn't handle #defines


;;; Compile

(defun nvp-makefile-save-and-compile (&optional arg)
  "Save and compile.
With prefix ARG, run `helm-make'."
  (interactive "P")
  (save-buffer)
  (if arg (call-interactively 'helm-make)
    (call-interactively 'nvp-compile))
  (pop-to-buffer next-error-last-buffer))

;; modified `helm--make-target-list-qp'
;; read targets from 'make -prqnR' output
(defun nvp-makefile-targets--make (makefile)
  (let ((dir (if (file-directory-p makefile) makefile
               (file-name-directory makefile)))
        target targets)
    (with-temp-buffer
      (insert (shell-command-to-string (concat "make -prqnRs -C " dir)))
      (goto-char (point-min))
      ;; (re-search-forward "^# Files")
      (while (re-search-forward "^\\([^#:\n\t ]+\\):\\([^=]\\|$\\)" nil t)
        (setq target (match-string-no-properties 1))
        (unless (or (save-excursion
                      (goto-char (match-beginning 0))
                      (forward-line -1)
                      (looking-at "^# Not a target:"))
                    (string-match "^\\([/a-zA-Z0-9_. -]+/\\)?\\." target))
          (push target targets)))
      targets)))

;; read targets from source
(defun nvp-makefile-targets--source (makefile)
  (let (targets)
    (with-temp-buffer
      (insert-file-contents makefile)
      (goto-char (point-min))
      (while (re-search-forward "^\\([^#: \n]+\\):" nil t)
        (let ((str (match-string 1)))
          (unless (string-match "^\\." str)
            (push str targets)))))
    (nreverse targets)))

;; Special targets: collect matches from url
(defun nvp-makefile-collect-topics (url regex)
  (let (res)
    (nvp-while-scanning-url url regex
      (push (match-string-no-properties 1) res))
    res))

(nvp-define-cache-runonce nvp-makefile-special-targets ()
  "List of special make targets."
  ;; propertize :manual (concat url (match-string 1))
  (nvp-makefile-collect-topics
   "https://www.gnu.org/software/make/manual/html_node/Special-Targets.html"
   "dt[>< ]+code[<> ]+\\([.A-Za-z]+\\)"))

;;; Indent
(defvar nvp-makefile-indent-offset 2)

;; indent ifeq ... endif regions
(defun nvp-makefile-indent (&optional start finish)
  (or finish (setq finish (point-max)))
  (save-excursion
    (goto-char (or start (point-min)))
    (while (re-search-forward (car nvp-makefile-open/close) finish t)
      (let ((beg (match-beginning 0)) end)
        (when (re-search-forward
               (concat "^else\\|" (cadr nvp-makefile-open/close)) finish t)
          (setq end (nvp-point 'boll))
          (goto-char beg)
          (while (and (nvp-goto 'bonll)
                      (< (point) end)
                      (not (looking-at makefile-dependency-regex)))
            (delete-horizontal-space)
            (indent-to-column nvp-makefile-indent-offset))
          (goto-char end))))))

;;; Tidy

(defun nvp-makefile-format-buffer (&optional beg end)
  (interactive "r")
  (unless (and (use-region-p) beg end (> end beg))
    (setq beg (point-min)
          end (point-max)))
  (nvp-makefile-indent beg end)
  (align nil beg end))

;; cleanup buffer before save
(defun nvp-makefile-cleanup-buffer ()
  (unless (or buffer-read-only (not (buffer-modified-p)))
    t
    ;; fixup indent
    ;; (nvp-makefile-indent)
    ;; align [?:]= before first rule
    ;; (align (point-min) (point-max))     ;use builtin align rules for now
    ;; (let ((end (save-excursion
    ;;              ;; find first rule
    ;;              (progn (goto-char (point-min))
    ;;                     (re-search-forward "^[^ ]+:" nil t)
    ;;                     (point)))))
    ;;   (align-regexp (point-min) end (nvp-concat
    ;;                                  "\\(?:[^<?]\\)\\(\\s-*\\)"
    ;;                                  "\\(=\\|[:?+]=\\)")
    ;;                 1))
    ;; align trailing '\'
    ;; (align-regexp (point-min) (point-max) "\\(\\s-*\\)\\\\\\s-*$")
    ))

(provide 'nvp-makefile)
;;; nvp-makefile.el ends here
