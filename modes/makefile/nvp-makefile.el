;;; nvp-makefile.el --- make helpers -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; make -q foo => exit 0 if foo is up-to-date
;; - align rules added in nvp-align:
;;   similar to sh-mode rules => equals, EOL comments/backslashes
;; - adds beg/end of defuns
;; - additional font-locking
;; - completing read for compile targets
;; - formatting/tidy
;;
;; TODO:
;; - make compile generic
;; - better additional font-locking: shell/define/info/warn/error
;; - fold: directives, rules, comments
;;
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)                          ; `nvp-block-face'
(require 'make-mode)
(nvp:req 'nvp-makefile 'subrs)
(nvp:auto "align" align-region)

(nvp:decls :p (helm crm compilation)
           :f (nvp-makefile-indent compilation-read-command)
           :v (align-rules-list align-exclude-rules-list))

(nvp:package-define-root :name "nvp-makefile")

(defun nvp-makefile-target-name ()
  (save-excursion
    ;; forward one line so if point on target line
    ;; the target in the current line is toggled
    (forward-line 1)
    (makefile-previous-dependency)
    ;; `makefile-previous-dependency' modifies match-data
    ;; with `looking-at'
    (string-trim (match-string-no-properties 1))))

;;; Navigation

(eval-and-compile
  (defconst nvp-makefile-open/close
    (let ((openers (rx bol (or "ifeq" "ifneq" "ifdef" "ifndef" "define")))
          (closers (rx bol (or "endif" "endef"))))
      (list openers closers))))

(defconst nvp-makefile-defun-regexp
  (nvp:concat (car nvp-makefile-open/close) "\\|" "^[^# \t\n]+:"))

(defun nvp-makefile--match-opener (search-fn)
  (beginning-of-line)
  (let ((index (if (eq search-fn 're-search-backward) (cons 1 0)
                 (cons 0 1))))
    (if (looking-at (concat "^else\\|" (nth (car index) nvp-makefile-open/close)))
        (funcall search-fn (nth (cdr index) nvp-makefile-open/close) nil t))))

(defun nvp-makefile--beginning-of-defun (arg)
  (let ((search-fn (if (> arg 0) #'re-search-backward #'re-search-forward))
        (pos (point-marker)))
    (and (< arg 0)                   ;searching forward -- skip initial beg .
         (nvp:makefile--defun-line-p)
         (end-of-line))
    (while (and (funcall search-fn nvp-makefile-defun-regexp nil 'move)
                (not (nvp:makefile--skip-escapes search-fn))))
    (if (nvp:makefile--defun-line-p)
        (or (nvp:point 'boll) (point))  ;found beg
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
                (nvp:goto 'bonll)
                (not (looking-at-p
                      (concat "^\\s-*$\\|" nvp-makefile-defun-regexp)))))
    (point)))

;; -------------------------------------------------------------------
;;; Font-lock

(defface makefile-shell
  '((t (:inherit (nvp-block-face) :extend t)))
  "Shell face."
  :group 'makefile)

;; TODO: remove string fontification in #define blocks where it is incorrect.
;; better to fontify using `forward-sexp' to allow for closing parens in command
(nvp:font-lock-add-defaults 'makefile-gmake-mode
  ("\\$(\\s-*info\\s-*\\([^)]*\\)" (1 'nvp-info-face prepend))
  ("\\$(\\s-*warning\\s-*\\([^)]*\\)" (1 'nvp-warning-face prepend))
  ("\\$(\\s-*error\\s-*\\([^)]*\\)" (1 'nvp-error-face prepend))
  ("\\(^\\|[^\\]\\)\\(\\\\\\\\\\)*\\(\\\\\\)$" (3 'nvp-line-escape-face)))

;; `makefile-dependency-regex' => note this doesn't take into account quoting
;; `makefile-macroassign-regex' => doesn't handle #defines

;; -------------------------------------------------------------------
;;; Compile

;; modified `helm--make-target-list-qp' in helm-make read targets from 'make
;; -prqnR' output
(defun nvp-makefile-targets--make (makefile)
  (let (target targets)
    (with-temp-buffer
      (insert (shell-command-to-string
               (format "make -prqnRs %s %s"
                       (if (file-directory-p makefile) "-C" "-f")
                       makefile)))
      (goto-char (point-min))
      ;; (re-search-forward "^# Files")
      (while (re-search-forward "^\\([^#:\n\t ]+\\):\\([^=]\\|$\\)" nil t)
        (setq target (match-string-no-properties 1))
        (unless (or (save-excursion
                      (goto-char (match-beginning 0))
                      (forward-line -1)
                      (looking-at "^# Not a target:"))
                    (string-match "^\\([/a-zA-Z0-9_. -]+/\\)?\\." target))
          (push target targets))))
    targets))

(defun nvp-makefile-targets--source (makefile)
  "Return targets defined in MAKEFILE."
  (let (targets)
    (with-temp-buffer
      (insert-file-contents makefile)
      (goto-char (point-min))
      (while (re-search-forward "^\\([^#: \n]+\\):" nil t)
        (let ((str (match-string 1)))
          (unless (string-match "^\\." str)
            (push str targets)))))
    (nreverse targets)))

(defun nvp-makefile-completing-read (makefile &optional prompt)
  "Completing read for multiple targets from MAKEFILE.
MAKEFILE should be a Makefile buffer or filename."
  (let ((crm-separator "[ 	]*,[ 	]*"))
    (completing-read-multiple
     (or prompt "Targets(','separated): ")
     (if (and (bufferp makefile)
              (with-current-buffer makefile
                (derived-mode-p 'makefile-mode)))
         (with-current-buffer makefile
           (when (derived-mode-p 'makefile-mode)
             (let ((makefile-need-target-pickup t))
               (makefile-pickup-targets)
               makefile-target-table)))
       (when (file-exists-p makefile)
         (nvp-makefile-targets--make makefile))))))

(defun nvp-makefile-save-and-compile (targets &optional arg)
  "Save and compile.
With prefix ARG, run `helm-make' if bound or allow editing command to run
using `compilation-read-command'."
  (interactive
   (progn (save-buffer)
          (list (and (not (and current-prefix-arg (fboundp 'helm-make)))
                     (nvp:makefile-read-targets))
                current-prefix-arg)))
  (if (and arg (fboundp 'helm-make)) (call-interactively #'helm-make)
    (let ((command (concat "make -f " (buffer-file-name) " " targets)))
      (nvp:makefile-with-compilation-vars
       (compilation-start (if current-prefix-arg
                              (compilation-read-command command)
                            command))))))

(defun nvp-makefile-run-target ()
  "Run target at point."
  (interactive)
  (when-let ((target (nvp-makefile-target-name)))
    (nvp-makefile-save-and-compile target)))


(defun nvp-makefile-format-buffer (&optional beg end)
  "Indent and align things in buffer."
  (interactive (if (region-active-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (require 'align)
  (nvp-makefile-indent beg end)
  (let (indent-tabs-mode)
    (align-region beg end "^\\s-*$" align-rules-list align-exclude-rules-list)))

(provide 'nvp-makefile)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-makefile.el ends here
