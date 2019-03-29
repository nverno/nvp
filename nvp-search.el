;;; nvp-search.el --- search/replace -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-29 01:53:40>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 13 February 2019

;;; Commentary:
;; todo ag / grep / find all c includes for project
;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'hydra)
  (require 'nvp-macro)
  (nvp-local-vars))
(nvp-declare "wgrep" wgrep-exit wgrep-save-all-buffers wgrep-abort-changes
  wgrep-remove-change wgrep-remove-all-change wgrep-toggle-readonly-area
  wgrep-mark-deletion wgrep-change-to-wgrep-mode)

(defvar nvp-search-history () "Store search history inputs.")

;; -------------------------------------------------------------------
;;; Rgrep

;;;###autoload
(defun nvp-rgrep-symbol (arg)
  "Lookup SYM using rgrep with FILES regexp from ROOT directory.
By default, lookup symbol at point in files matching current file's extension
or '*' from emacs root, ignoring package directory.
(1) prefix, includes package directory.
(2) prefix just calls `rgrep' on all files (no ext).
(3) prefix prompt for confirmation"
  (interactive "P")
  (require 'nvp-grep-config)
  (let ((sym (or (thing-at-point 'symbol t)
                 (read-string "Symbol: " nil 'nvp-search-history)))
        (ext (and (/= (prefix-numeric-value arg) 16)
                  (file-name-extension (or (buffer-file-name) (buffer-name)))))
        (grep-find-ignored-directories
         (if (not (equal '(4) arg))
             (cons (file-name-nondirectory package-user-dir)
                   grep-find-ignored-directories)
           grep-find-ignored-directories)))
    (grep-compute-defaults)
    (rgrep (format "[ '\\(]%s[ \\)\\t\\n\\b]*" sym)
           (if ext (concat "*." ext) "*")
           nvp/emacs (equal '(64) current-prefix-arg))))

;; -------------------------------------------------------------------
;;; ag.el
;;
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
(nvp-declare "ag" ag-project-dired ag-dired-regexp ag-dired
  ag-project-dired-regexp ag/search)
(defvar ag/file-column-pattern-group)
(defvar ag-ignore-list)
(defvar nvp-ag-grouped-file-regex "^\\(?:File:\\s-*\\)\\([^ \t].*\\)$")
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

;;;###autoload
(defun nvp-ag (str dir &optional regex)
  "Search for STR from root DIR using ag.
Defaults to symbol at point and emacs root.
(1) prefix prompts for STR and DIR
(2) or more prefix args to treat STR as REGEX
(3) prefix prompt and treat as REGEX"
  (interactive
   (let* ((arg (prefix-numeric-value current-prefix-arg))
          (sym (thing-at-point 'symbol t))
          (dir nvp/emacs)
          (re (> (prefix-numeric-value current-prefix-arg) 4))
          (elpa (file-name-nondirectory
                 (directory-file-name user-package-dir))))
     (if (< arg 16)
         (cl-pushnew elpa ag-ignore-list :test #'equal)
       (cl-callf2 cl-delete elpa ag-ignore-list :test #'equal))
     (cond
      ((memq arg '(4 16))
       (list (read-string
              (format "Ag search%s: " (if sym (concat " ('" sym "')") ""))
              nil 'nvp-search-history sym)
             (read-directory-name
              "Search directory(~/.emacs.d): " nvp/emacs nvp/emacs t "~/")
             re))
      ((null sym)
       (list (read-string "Ag search string: " nil 'nvp-search-history)
             nvp/emacs re))
      (t
       (add-to-history 'nvp-search-history sym)
       (list sym dir re)))))
  (require 'ag)
  ;; just unset these to work with wgrep-ag
  (let (compilation-environment
        compilation-start-hook)
   (ag/search str dir :regexp regex)))

;;;###autoload
(defun nvp-ag-dired (arg)
  "Call ag dired functions.
(0) `ag-project-dired-regexp'
(1) `ag-project-dired'
(2) `ag-dired-regexp'
(3) `ag-dired'" 
  (interactive (list (prefix-numeric-value current-prefix-arg)))
  (require 'ag)
  (pcase arg
    (`4 (call-interactively #'ag-project-dired))
    (`16 (call-interactively #'ag-dired-regexp))
    (`64 (call-interactively #'ag-dired))
    (_ (call-interactively #'ag-project-dired-regexp))))

;; -------------------------------------------------------------------
;;; wgrep

;;;###autoload
(defun nvp-wgrep-bind ()
  "Bind wgrep in current mode."
  (interactive)
  (require 'nvp-grep-config)
  (let ((map (symbol-value (intern-soft (format "%s-map" major-mode)))))
    (define-key map (kbd "C-x C-n w") #'wgrep-change-to-wgrep-mode)))

;;;###autoload(autoload 'nvp-wgrep-hydra/body "nvp-search")
(nvp-hydra-set-property 'nvp-wgrep-hydra)
(defhydra nvp-wgrep-hydra (:color red)
  ("q" wgrep-exit "exit")
  ("s" wgrep-save-all-buffers "save all")
  ("a" wgrep-abort-changes "abort")
  ("r" wgrep-remove-change "remove region change")
  ("R" wgrep-remove-all-change "remove all changes")
  ("t" wgrep-toggle-readonly-area "toggle r/o")
  ("m" wgrep-mark-deletion "mark deletion"))

(provide 'nvp-search)
;;; nvp-search.el ends here
