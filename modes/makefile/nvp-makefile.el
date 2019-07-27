;;; nvp-makefile.el --- make helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; make -q foo => exit 0 if foo is up-to-date

;; TODO:
;; - incorporate semantic stuff?
;; - use info-completition-at-point function #<marker at 25312 in info-look.el.gz>
;; - align rules => similar to sh-mode rules

;; FIXME:
;; - beg/end account for if/endif, define, escaped lines
;; - collect remote info async

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro)
  (require 'subr-x))
(require 'nvp)
(require 'make-mode)
(nvp-decls)

;; -------------------------------------------------------------------
;;; Completion

;; (defun nvp-makefile-env-table ()
;;   (with-temp-buffer
;;     (insert
;;      (shell-command-to-string
;;       (format "make -nspf %s | awk 'f{print; f=0} /^# environment|^# makefile/{f=1}"
;;               (buffer-file-name))))
;;     (goto-char (point-min))
;;     ()))

;; -------------------------------------------------------------------
;;; Things-at-point
;; macro => `make-macro' `macrostep-make-bounds-of-macro-at-point'

;; -------------------------------------------------------------------
;;; Font-lock
;; TODO:
;; - remove string fontification in #define blocks where it is incorrect.

;; better to fontify using `forward-sexp' to allow for closing parens in command
(nvp-font-lock-add-defaults 'makefile-gmake-mode
  ("\\$(\\s-*info\\s-*\\([^)]*\\)" (1 'nvp-info-face prepend))
  ("\\$(\\s-*warning\\s-*\\([^)]*\\)" (1 'nvp-warning-face prepend))
  ("\\$(\\s-*error\\s-*\\([^)]*\\)" (1 'nvp-error-face prepend)))

;; ------------------------------------------------------------
;;; Web topics

;; FIXME: run async
;; collect matches from url
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

;; -------------------------------------------------------------------
;;; General mode variables

(defsubst nvp-makefile--at-beginning ()
  (save-excursion
    (beginning-of-line 1)
    (looking-at-p "^[^#\t\n ]")))

(defun nvp-makefile--beginning-of-defun (arg)
  (let ((search-fn (if (> arg 0) #'re-search-backward #'re-search-forward))
        (pos (point-marker)))
    (and (< arg 0)                          ;searching forward -- skip initial beg. 
         (nvp-makefile--at-beginning)
         (end-of-line 1))
    (funcall search-fn "^[^#\t\n ]" nil 'move)
    (if (nvp-makefile--at-beginning)
        (or (beginning-of-line 1) (point))  ;found beg
      (and (goto-char pos) nil))))          ;failed

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
  (when (or (nvp-makefile--at-beginning)
            (nvp-makefile-beginning-of-defun 1)
            (nvp-makefile-beginning-of-defun -1))
    (beginning-of-line)
    (forward-line)
    (while (and (not (eobp))
                (looking-at-p "^#\\|^[ \t]+"))
      (forward-line))))

;; -------------------------------------------------------------------
;;; Indent 

(defvar nvp-makefile-indent-offset 2)

;; indent ifeq ... endif regions
(defun nvp-makefile-indent ()
  (save-excursion
    (goto-char (point-min))
    ;; get first rule
    (let ((end (save-excursion
                 (progn
                   (re-search-forward makefile-dependency-regex nil t)
                   (point)))))
      (while (search-forward "ifeq" end 'move)
        ;; indent if block
        (forward-line 1)
        (let ((close (save-excursion
                       (search-forward "endif")
                       (line-number-at-pos))))
          (while (< (line-number-at-pos) close)
            (beginning-of-line)
            (unless (looking-at-p "\\s-*else")
              (delete-horizontal-space)
              (indent-to nvp-makefile-indent-offset))
            (forward-line 1)))))))

;; -------------------------------------------------------------------
;;; Font-locks
;; `makefile-dependency-regex' => note this doesn't take into account quoting
;; `makefile-macroassign-regex' => doesn't handle #defines

;; (let ((io-funs
;;        (concat "\\${\\s-*" (regexp-opt '("info" "warn" "error"))
;;                "\\(.*\\)}")))
;;   `(nvp-font-lock-add-defaults 'makefile-mode
;;      ("\\${\\s-*}")
;;     ))

;; -------------------------------------------------------------------
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

;; ------------------------------------------------------------
;;; Hooks

;; cleanup buffer before save
(defun nvp-makefile-cleanup-buffer ()
  (unless (or buffer-read-only (not (buffer-modified-p)))
    ;; fixup indent
    (nvp-makefile-indent)
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
