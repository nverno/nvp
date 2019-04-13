;;; nvp-macs-common.el --- basic macros -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-04-13.00>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 30 March 2019

;;; Commentary:
;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'macroexp)

;; -------------------------------------------------------------------
;;; Helpers

(defun nvp--normalize-modemap (mode &optional minor)
  "Convert MODE to keymap symbol if necessary.
If MINOR is non-nil, create minor mode map symbol."
  (and (eq 'quote (car-safe mode)) (setq mode (eval mode)))
  (if (keymapp mode) mode
    (and (symbolp mode) (setq mode (symbol-name mode)))
    (let ((minor (or minor (string-match-p "-minor" mode))))
      (if (not (or (string-match-p "-map\\'" mode)
                   (string-match-p "-keymap\\'" mode)))
          (intern
           (concat (replace-regexp-in-string "\\(?:-minor\\)?-mode\\'" "" mode)
                   (if minor "-minor-mode-map" "-mode-map")))
        (intern mode)))))

(defun nvp--normalize-hook (mode &optional minor)
  "Convert MODE to canonical hook symbol.
If MINOR is non-nil, convert to minor mode hook symbol."
  (and (eq 'quote (car-safe mode)) (setq mode (eval mode)))
  (and (symbolp mode) (setq mode (symbol-name mode)))
  (let ((minor (or minor (string-match-p "-minor" mode))))
    (intern
     (concat
      (replace-regexp-in-string
       "\\(?:-minor-\\)?\\(?:-mode\\)?\\(?:-hook\\)?\\'" "" mode)
      (if minor "-minor-mode-hook" "-mode-hook")))))

;; -------------------------------------------------------------------
;;; OS 

(defmacro nvp-with-w32 (&rest body)
  (declare (indent 0) (debug t))
  (when (eq system-type 'windows-nt)
    `(progn ,@body)))

(defmacro nvp-with-gnu (&rest body)
  (declare (indent 0) (debug t))
  (when (not (eq system-type 'windows-nt))
    `(progn ,@body)))

(defmacro nvp-with-gnu/w32 (gnu w32)
  (declare (indent 2) (indent 1) (debug t))
  (if (eq system-type 'windows-nt)
      `,@w32
    `,@gnu))

;; -------------------------------------------------------------------
;;; Compat 

(defvar eieio--known-slot-names)
(defmacro eieio-declare-slot (name)
  (cl-pushnew name eieio--known-slot-names) nil)

;; -------------------------------------------------------------------
;;; Conversion

;; unquote, unfunction, all elements in args - return as list
;; eg. '(#'a b 'c) => '(a b c), or #'fn => '(fn), or ('a #'b) => '(a b)
(defsubst nvp--unquote (args)
  (while (memq (car-safe args) '(function quote))
    (setq args (cadr args)))
  (delq nil (if (listp args)
                (cl-loop for arg in args
                   do (while (memq (car-safe arg) '(function quote))
                        (setq arg (cadr arg)))
                   collect arg)
              (cons args nil))))

(defmacro nvp-listify (&rest args)
  "Ensure all items in ARGS are lists."
  `(progn
     ,@(mapcar (lambda (arg)
                 (and (stringp arg) (setq arg (intern-soft arg)))
                 `(unless (and ,arg
                               (listp ,arg)
                               (not (functionp ,arg)))
                    (setq ,arg (list ,arg))))
               args)))

(defmacro nvp-string-or-symbol (sym)
  "If SYM is string convert to symbol."
  `(if (stringp ,sym) (intern ,sym) ,sym))

(defmacro nvp-stringify (name)
  "Sort of ensure that NAME symbol is a string."
  `(progn
     (pcase ,name
       ((pred stringp) ,name)
       ((pred symbolp) (symbol-name ,name))
       (`(quote ,sym) (symbol-name sym))
       (`(function ,sym) (symbol-name sym))
       (_ (user-error "How to stringify %S?" ,name)))))

;; -------------------------------------------------------------------
;;; Declares / Autoloads
;; silence byte-compiler warnings

(defalias 'nvp-decl 'nvp-declare)
(put 'nvp-decl 'lisp-indent-function 1)

(cl-defmacro nvp-declare (&rest funcs &key pre &allow-other-keys)
  (declare (indent defun))
  (setq pkg (or (cl-getf funcs :pkg) ""))
  (while (keywordp (car funcs))
    (setq funcs (cdr (cdr funcs))))
  (setq funcs (nvp--unquote funcs))
  (when pre
    (setq funcs (mapcar (lambda (fn)
                          (let ((fn (if (symbolp fn) (symbol-name fn) fn)))
                            (if (string-prefix-p pre fn) fn
                              (intern (concat pre "-" fn)))))
                        funcs)))
  (macroexp-progn
   (cl-loop for func in funcs
      collect `(declare-function ,func ,pkg))))

(defmacro nvp-autoload (package &rest funcs)
  (declare (indent defun))
  (setq funcs (nvp--unquote funcs))
  (macroexp-progn
   (cl-loop for func in funcs
      collect `(autoload ',func ,package))))

;; -------------------------------------------------------------------
;;; Files / buffers

(defmacro nvp-file-same (file-1 file-2)
  "Return non-nil if FILE-1 and FILE-2 are the same."
  (declare (indent defun))
  `(when (and (file-exists-p ,file-1) (file-exists-p ,file-2))
     (equal (file-truename ,file-1) (file-truename ,file-2))))

;; modified from smartparens.el
;; (defmacro nvp-with-buffers-using-mode (mode &rest body)
;;   "Execute BODY in every existing buffer using `major-mode' MODE."
;;   (declare (indent 1))
;;   `(dolist (buff (buffer-list))
;;      (when (provided-mode-derived-p ,mode (buffer-local-value 'major-mode buff))
;;        (with-current-buffer buff
;;          ,@body))))

;;; Load file name
(defmacro nvp-load-file-name ()
  "Expand to the file's name."
  '(cond
    (load-in-progress load-file-name)
    ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
     byte-compile-current-file)
    (t (buffer-file-name))))

;; -------------------------------------------------------------------
;;; General

;; not that useful -- concat only happens one first load
(defmacro nvp-concat (&rest body)
  `(eval-when-compile (concat ,@body)))

(defmacro nvp-re-opt (opts &optional no-symbol)
  `(eval-when-compile
     (concat ,(and (not no-symbol) "\\_<") (regexp-opt ,opts t)
             ,(and (not no-symbol) "\\_>"))))

;; -------------------------------------------------------------------
;;; Input keys / IO

;; FIXME: best way to get last input char?
;; `key-description' is defined at C level and `edemacro-format-keys' does
;; a lot of work. How to profile?
;; - semantic-read-event : #<marker at 3072 in fw.el.gz>
(defmacro nvp-input (type)
  "Return user input by TYPE.
See Info node `(elisp) Input Events'.

* ~~~ Last command keys
`lce'  -- Last key entered during `last-command-event' with ctrl chars stripped.
`lcef' -- Full key from `last-command-event', possibly with meta chars.
`lic'  -- Last input char using `edemacro-format-keys' with `last-input-event'.
`licf' -- Full key from `last-input-event' using `edmacro-format-keys'.

* ~~~ Last command names
`lrc'  -- For now, just `last-repeatable-command' - probably should filter out
          uselss commands
`tck'  -- Last key from `this-command-keys-vector'.

* ~~~ Last event 
`em'   -- event modifiers of `last-command-event'
`ebt'  -- `event-basic-type' of `last-command-event'
"
  (let ((type (eval type)))
    (cond
     ;; === Last input key ===
     ((eq type 'lce)
      `(substring (key-description (vector last-command-event)) -1))
     ((eq type 'lcef)
      `(key-description (vector last-command-event)))
     ((eq type 'lic)
      '(kbd (substring (edmacro-format-keys (vector last-input-event)) -1)))
     ((eq type 'licf)
      '(kbd (edmacro-format-keys (vector last-input-event))))

     ;; === Events ===
     ((eq type 'em) (event-modifiers last-command-event))
     ((eq type 'ebt) (event-basic-type last-command-event))
     ((eq type 'ec) (event-click-count last-command-event))
     
     ;; === Last command (symbol) ===
     ((eq type 'lrc) last-repeatable-command)
     ((eq type 'tck)
      '(let* ((keys (this-command-keys-vector))
              (last-key (and (vectorp keys)
                             (aref keys (1- (length keys))))))
         (and last-key (lookup-key (current-active-maps 'olp) (vector last-key)))))

     (t (message "Unknown type `nvp-input': %S" type)))))

(provide 'nvp-macs-common)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-macs-common.el ends here
