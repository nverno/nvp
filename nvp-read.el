;;; nvp-read.el --- Completing read for thangs -*- lexical-binding: t; -*-

;;; Commentary:

;; various completing read functions
;;; TODO:
;; - read w/ popup help: see `register-read-with-preview'

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)                     ;string-remove-suffix
  (require 'nvp-macro)
  (require 'nvp))
(require 'nvp)                          ;nvp-prompt-default
(declare-function function-called-at-point "help")
(declare-function help--symbol-completion-table "help-fns")
(autoload 'eldoc-minibuffer-message "eldoc")

;; just MODE's name minus the "-mode"
(defsubst nvp-read--mode-name (&optional mode)
  (if mode
      (and (symbolp mode) (setq mode (symbol-name mode)))
    (setq mode (symbol-name major-mode)))
  (string-remove-suffix "-mode" mode))

;; list filenames relative to ROOT matching REGEXP
(defsubst nvp-read--relative-files (root regexp)
  (mapcar (lambda (f) (file-relative-name f root))
          (directory-files-recursively root regexp)))

;;;###autoload
(defun nvp-read-relative-recursively (root regexp &optional prompt default)
  "Return full filepath prompting with file matching REGEXP from ROOT with\
`directory-files-recursively'."
  (expand-file-name
   (nvp-completing-read
    (nvp-prompt-default (or prompt "File: ") default)
    (nvp-read--relative-files root regexp) nil nil nil
    'nvp-read-config-history default)
   root))

;; some completing reads for general config files
(defun nvp-read-mode-config (&optional prompt default)
  (unless default
    (setq default (symbol-name major-mode)))
  (setq prompt (nvp-prompt-default (or prompt "Mode config: ") default))
  (catch 'dired
    (nvp-completing-read
     prompt
     (mapcar
      #'(lambda (x) ;; ignore preceding 'nvp-' and ending '-config.el'
          (replace-regexp-in-string "\\(nvp-\\|\\(?:-config\\)?\\.el\\)" "" x))
      (directory-files nvp/config nil "^[^\\.].*\\.el$"))
     nil nil nil 'nvp-read-config-history (nvp-read--mode-name default))))

(defun nvp-read--info-files (&optional prompt default)
  (or default (and (string-prefix-p nvp/info default-directory)
                   (setq default (nvp-path 'bfs))))
  (expand-file-name 
   (nvp-completing-read
    (nvp-prompt-default (or prompt "Info file: ") default)
    (directory-files (expand-file-name "org" nvp/info) nil "\.org")
    nil nil nil 'nvp-read-config-history default)
   (expand-file-name "org" nvp/info)))

(defun nvp-read--mode-test (&optional prompt default)
  (let* ((ext (ignore-errors (nvp-path 'ext)))
         (default-directory nvp/test)
         (completion-ignored-extensions
          (cons "target" completion-ignored-extensions))
         (files (nvp-read--relative-files nvp/test "^[^.][^.]")))
    (unless default
      (setq default (and ext (cl-find-if (lambda (f) (string-suffix-p ext f)) files))))
    (setq prompt (nvp-prompt-default (or prompt "Test: ") default))
    (expand-file-name
     (nvp-completing-read
      prompt files nil nil nil 'nvp-read-config-history default)
     nvp/test)))

;; if LOCAL is non-nil use that
(defun nvp-read--org-file (&optional prompt default nolocal)
  (when (derived-mode-p 'comint-mode)   ;jumping from shell
    (hack-local-variables))             ;read .dir-locals.el if exist
  (let ((local (bound-and-true-p nvp-local-notes-file)))
    (if (and local (not nolocal)) local
      (or default (setq default nvp-default-org-file))
      (setq prompt (nvp-prompt-default (or prompt "Org file: ") default))
      (nvp-read-relative-recursively
       nvp/org "\.org$" (or prompt "Org file: ") default))))

;; read mode installation files
(defun nvp-read--mode-install (&optional mode prompt default)
  (or prompt (setq prompt "Install file: "))
  (let* ((mode (nvp-read--mode-name mode))
         (modedir (expand-file-name mode nvp/install))
         files)
    (if (not (file-exists-p modedir))
        (nvp-read-relative-recursively nvp/install "^[^.]+$" prompt)
      (setq files (directory-files modedir nil "^[^.]+$"))
      (if (eq 1 (length files))
          (expand-file-name (concat mode "/" (car files)) nvp/install)
        (unless default
          (setq prompt
                (nvp-prompt-default
                 prompt (cl-some (lambda (f)
                                   (member
                                    f `(,(format "install-%s" mode) "install")))
                                 files))))
        (expand-file-name
         (concat
          mode "/"
          (nvp-completing-read
           prompt files nil nil nil 'nvp-read-config-history default))
         nvp/install)))))

;; -------------------------------------------------------------------
;;; Minibuffer input

;;;###autoload
(defun nvp-read-with-message (prompt &optional format-string &rest args)
  "Display message in mode-line while reading from minibuffer."
  (minibuffer-with-setup-hook
      (:append (lambda () (eldoc-minibuffer-message format-string args)))
    (read-from-minibuffer prompt)))

;; -------------------------------------------------------------------
;;; Elisp objects 

(defun nvp-read-keymap ()
  "Read keymap from `obarray'."
  (intern (completing-read "Keymap: " obarray
                           (lambda (m)
                             (and (boundp m)
                                  (keymapp (symbol-value m))
                                  (not (equal (symbol-value m)
                                              (make-sparse-keymap)))))
                           t nil nil nil)))

(defun nvp-read-nvp-keymap ()
  "Read one of my keymaps."
  (intern (completing-read "nvp-..-keymap (default nvp-keymap): " obarray
                           (lambda (m)
                             (and (boundp m)
                                  (keymapp (symbol-value m))
                                  (string-prefix-p "nvp-" (symbol-name m))))
                           t nil 'nvp-read-config-history "nvp-keymap")))

(eval-when-compile
  (defmacro nvp-read--default (default &rest body)
    (macroexp-let2 nil def default
     `(if (eq ,def :none) nil
        (or ,def ,@body)))))

(defun nvp-read-obarray-regex (prompt &optional regexp default hist)
  "Completing read for obarray with optional REGEXP filter."
  (completing-read prompt obarray
                   (lambda (sym) (string-match-p regexp (symbol-name sym)))
                   t nil hist
                   (and default (if (symbolp default) (symbol-name default)
                                  default))))

;; #<marker at 34938 in help-fns.el.gz>
(defun nvp-read-elisp-symbol (prompt &optional predicate default hist)
  "Read symbol using `help--symbol-completion-table' using PROMPT with DEFAULT.
Filter by PREDICATE if non-nil."
  (require 'help-fns)
  (setq default (nvp-read--default default (nvp-tap 'tap)))
  (let ((enable-recursive-minibuffers t) val)
    (setq prompt (nvp-prompt-default prompt default))
    (setq val (completing-read prompt #'help--symbol-completion-table
                               predicate t nil hist
                               (when default
                                 (if (symbolp default) (symbol-name default)
                                   default))))
    (unless (equal val "")
      (intern val))))

(defun nvp-read-elisp-variable (prompt &optional default hist)
  "Lookup elisp symbol using PROMPT and optional DEFAULT."
  (setq default (nvp-read--default default (let ((var (variable-at-point)))
                                             (and (symbolp var) var))))
  (let ((orig-buffer (current-buffer)))
    (nvp-read-elisp-symbol prompt
                           (lambda (vv)
                             (with-current-buffer orig-buffer
                               (or (get vv 'variable-documentation)
                                   (and (boundp vv) (not (keywordp vv))))))
                           (or default :none) hist)))

(defun nvp-read-elisp-function (prompt &optional default hist)
  "Lookup elisp function with PROMPT and optional DEFAULT."
  (setq default (nvp-read--default default (function-called-at-point)))
  (nvp-read-elisp-symbol
   prompt (lambda (f) (or (fboundp f) (get f 'function-documentation))) default hist))

;;;###autoload
(defun nvp-read-mode (&optional default)
  "Lookup name of mode using PROMPT and optional DEFAULT."
  (unless default (setq default major-mode))
  (let ((prompt (if default (format "Mode (default \"%s\"): " default)
                  "Mode: ")))
    (nvp-read-obarray-regex prompt "-mode\\'" default)))

(provide 'nvp-read)
;;; nvp-read.el ends here
