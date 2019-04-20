;;; nvp-search.el --- search/replace -*- lexical-binding: t; -*-

;;; Commentary:

;; TODO: ripgrep support

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'hydra)
  (require 'nvp-macro))
(nvp-declare :pre "wgrep" exit save-all-buffers abort-changes remove-change
  remove-all-change toggle-readonly-area mark-deletion change-to-wgrep-mode)

(defvar nvp-search-history () "Store search history inputs.")

;; -------------------------------------------------------------------
;;; Rgrep

(eval-when-compile
  (defmacro nvp-rgrep-with-defaults (elisp &rest body)
    "Defaults: symbol at point, file/buffer extension.
(1) prefix: if ELISP is non-nil includes `package-user-dir', otherwise uses HOME 
    for `default-directory'.
(2) prefix calls `rgrep' on all files, ignoring current file/buffer extension.
(3) prompts for confirmation"
    (declare (indent defun) (debug t))
    `(progn
       (require 'nvp-grep-config)
       (let ((sym (nvp-tap 'tapi nil nil nil :hist 'nvp-search-history))
             ,@(if elisp                                                     ; '(4)
                   '((grep-find-ignored-directories
                      (nvp-prefix 4 (cons (nvp-path 'ds package-user-dir)
                                          grep-find-ignored-directories)
                        grep-find-ignored-directories)))
                 '((default-directory
                     (nvp-prefix 4 (getenv "HOME") default-directory))))
             (ext (nvp-prefix 16 (nvp-path 'ext nil :or-name t) :test '/=))  ; '(16)
             (confirm (nvp-prefix 64)))                                      ; '(64)
         ,@(if body `,@body
             `((grep-compute-defaults)
               (rgrep (format "\\b%s\\b" sym)
                      (if ext (concat "*." ext "*"))
                      ,(if elisp nvp/emacs 'default-directory)
                      confirm)))))))

;;;###autoload
(defun nvp-rgrep-symbol-at-point (arg)
  "Rgrep for symbol at point. See `nvp-rgrep-with-defaults'."
  (interactive "P")
  (setq current-prefix-arg arg)
  (nvp-rgrep-with-defaults nil))

;;;###autoload
(defun nvp-rgrep-elisp-symbol-at-point (arg)
  "Rgrep for elisp symbol at point. See `nvp-rgrep-with-defaults'."
  (interactive "P")
  (setq current-prefix-arg arg)
  (nvp-rgrep-with-defaults 'elisp))


;; -------------------------------------------------------------------
;;; wgrep

;;;###autoload(autoload 'nvp-wgrep-hydra/body "nvp-search")
(nvp-hydra-set-property 'nvp-wgrep-hydra)
(defhydra nvp-wgrep-hydra (:color red)
  ("q" wgrep-exit "exit" :exit t)
  ("s" wgrep-save-all-buffers "save all")
  ("a" wgrep-abort-changes "abort" :exit t)
  ("r" wgrep-remove-change "remove region change")
  ("R" wgrep-remove-all-change "remove all changes")
  ("t" wgrep-toggle-readonly-area "toggle r/o")
  ("m" wgrep-mark-deletion "mark deletion"))


;; -------------------------------------------------------------------
;;; Ag

;; Notes
;; -----
;; `ag-filter' replaces escape sequences with 'File: ' by regex parsing
;; and then relies on that text to match `compilation-error-regexp's to jump
;; to error locations. However, this is fucked up by xterm-color's
;; `compilation-start-hook' which processes the string prior to passing it to
;; `compilation-filter', and then to `ag-filter'. It seems that
;; `xterm-color-filter' modifies the output so `ag-filter' no longer matches
;; and replaces text with 'File: ', hence the resulting compilation jumps
;; don't know the proper locations.
;;
;; Offending location: #<marker at 24916 in ag.el>. It seems like a very brittle
;; package and doesn't support wgrep. It also runs `shell-command' which
;; awkwardly invokes `shell-mode-hook' on the output results, which also
;; required fixes.
(nvp-decl ag-project-dired ag-dired-regexp ag-dired ag-project-dired-regexp
  ag/search)
(eval-when-compile
  (defvar ag/file-column-pattern-group)
  (defvar ag-ignore-list))
(defvar nvp-ag-grouped-file-regex "^\\(?:File:\\s-*\\)\\([^ \t].*\\)$"
  "Support either `xterm-color' filtered results or defaults.")

;;
;; Fixes for compilation:
;; (1) Can either disable xterm-color, which sucks
;; (define-advice ag/search (:around (orig-fn &rest args) "no-xterm-color")
;;   (let (compilation-start-hook)
;;     (apply orig-fn args)))
;; (2) Or override ag's `compilation-error-regexp-alist' matching function
(defun nvp-ag-match-grouped-filename ()
  "Match grouped filename in compilation output."
  (save-match-data
    (save-excursion
      (beginning-of-line)
      (while (and (not (bobp))
                  (looking-at-p ag/file-column-pattern-group))
        (forward-line -1))
      (and (looking-at nvp-ag-grouped-file-regex)
           (list (match-string 1))))))

(with-eval-after-load 'ag
  ;; override ag's function
  (setf (symbol-function 'ag/compilation-match-grouped-filename)
        #'nvp-ag-match-grouped-filename)

  (defvar nvp-ag-imenu-expression
    `((nil (lambda ()
             (cl-block nil
               (when (re-search-backward ,ag/file-column-pattern-group nil 'move)
                 (beginning-of-line 0)
                 (while (and (not (bobp))
                             (looking-at-p ,ag/file-column-pattern-group))
                   (forward-line -1))
                 (and (looking-at "^\\(?:File: \\)?\\([^ \t].*\\)$")
                      (cl-return t)))))
           1))))


(eval-when-compile
  (defmacro nvp-ag-with-defaults (elisp &rest body)
    "Search for STR from root DIR using ag.
Defaults to symbol at point and nvp/emacs if ELISP is non-nil, HOME otherwise.
If BODY is non-nil, it is executed in place of the default search.
(1) prefix prompts for STR and DIR
(2) or more prefix args to treat STR as REGEX
(3) prefix prompt and treat as REGEX"
    (declare (indent defun))
    `(progn
       (require 'nvp-ag-config)
       (let* ((dir (nvp-prefix '(4 64)
                     (read-from-minibuffer
                      "Root directory: " ,(if elisp 'nvp/emacs '(getenv "HOME")))
                     ,(if elisp 'nvp/emacs '(getenv "HOME"))))
              (sym (let ((tap (nvp-tap 'tap)))
                     (if (or (nvp-prefix '(4 64)) (null tap))
                         (read-from-minibuffer
                          (format "Ag search%s: "
                                  (if tap (concat " ('" tap "')") ""))
                          tap nil nil 'nvp-search-history tap)
                       (add-to-history 'nvp-search-history tap)
                       tap)))
              (re (nvp-prefix 4 nil :test '>))
              ,@(if elisp '((elpa (nvp-path 'ds package-user-dir)))
                  '((file-re (nvp-prefix 16))))
              ;; nullify to allow ag output parsing
              compilation-start-hook
              compilation-environment)
         (require 'ag)
         ,@(if elisp
               `((nvp-prefix 16 (cl-pushnew elpa ag-ignore-list :test #'equal)
                   :test '<
                   (cl-callf2 cl-delete elpa ag-ignore-list :test #'equal))))
         ,(if body `,@body
            ;; integer prefix is used as context argument
            ;; otherwise, prefix causes prompting with ag command
            '(unless (or (integerp current-prefix-arg)
                         (nvp-prefix 16 nil :test '>))
               (setq current-prefix-arg nil))
            (if elisp
                '(ag/search sym dir :regexp re)
              '(if file-re
                   (ag/search sym dir :file-regex sym)
                 (ag/search sym dir :regexp re))))))))

(defun nvp-ag-recompile ()
  (interactive)
  (let (compilation-start-hook)
    (call-interactively 'recompile)))


;;;###autoload
(defun nvp-ag-elisp-symbol-at-point (&optional arg)
  "Search for elisp symbol at point with ag. See `nvp-ag-with-defaults'."
  (interactive "P")
  (and arg (setq current-prefix-arg arg))
  (nvp-ag-with-defaults 'elisp))

;;;###autoload
(defun nvp-ag-symbol-at-point (&optional arg)
  "Search for symbol at point with ag. See `nvp-ag-with-defaults'."
  (interactive "P")
  (and arg (setq current-prefix-arg arg))
  (nvp-ag-with-defaults nil))

;;;###autoload
(defun nvp-ag-dired (arg)
  "Call ag dired functions.
(0) `ag-project-dired-regexp'
(1) `ag-project-dired'
(2) `ag-dired-regexp'
(3) `ag-dired'" 
  (interactive (list (prefix-numeric-value current-prefix-arg)))
  (require 'nvp-ag-config)
  (call-interactively
   (pcase arg
     (`4  #'ag-project-dired)
     (`16 #'ag-dired-regexp)
     (`64 #'ag-dired)
     (_   #'ag-project-dired-regexp))))

(provide 'nvp-search)
;;; nvp-search.el ends here
