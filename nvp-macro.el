;;; nvp-macro.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-15 10:19:50>
;; Package-Requires: 
;; Created:  2 November 2016

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'macroexp)
(defvar eieio--known-slot-names)

;; Doesn't work
;; (put 'require 'byte-hunk-handler 'byte-compile-file-form-require)
(when (functionp 'backtrace-frames)
  (when (assoc '(t byte-compile-file-form-require
                   ((require 'nvp-macro))
                   nil)
               (backtrace-frames))
    (message "Warning: package 'nvp-macro required at runtime")))

;; -------------------------------------------------------------------
;;; Utils

(defun nvp--normalize-modemap (mode &optional minor)
  "Convert MODE to keymap symbol if necessary.
If MINOR is non-nil, create minor mode map symbol."
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
  (and (symbolp mode) (setq mode (symbol-name mode)))
  (let ((minor (or minor (string-match-p "-minor" mode))))
    (intern
     (concat
      (replace-regexp-in-string
       "\\(?:-minor-\\)?\\(?:-mode\\)?\\(?:-hook\\)?\\'" "" mode)
      (if minor "-minor-mode-hook" "-mode-hook")))))

;; -------------------------------------------------------------------
;;; Misc

(defmacro nvp-defvar (var value)
  "Define VAR and eval VALUE during compile."
  (declare (indent defun))
  `(progn (defvar ,var (eval-when-compile ,value))))

(defmacro nvp-setq (var value)
  "Define VAR and eval VALUE during compile."
  (declare (indent defun))
  `(progn (eval-when-compile (defvar ,var))
          (setq ,var (eval-when-compile ,value))))

(defmacro current-buffer-process ()
  `(get-buffer-process (current-buffer)))

(defmacro nvp-bfn (&optional no-ext or-buffer)
  "Short buffer file name.
If NO-EXT is non-nil, remove file extension.  If OR-BUFFER is non-nil
use either `buffer-file-name' or `buffer-name'."
  (let ((buff (if or-buffer
                  '(or (buffer-file-name) (buffer-name))
                '(buffer-file-name))))
    `(file-name-nondirectory
      ,(if no-ext `(file-name-sans-extension ,buff)
        buff))))

(defmacro nvp-dfn (&optional with-default)
  "Short directory file name."
  (if with-default
      `(if buffer-file-name
           (file-name-nondirectory
            (directory-file-name
             (file-name-directory
              (file-name-sans-extension (buffer-file-name)))))
         (file-name-nondirectory (directory-file-name default-directory)))
    `(file-name-nondirectory
      (directory-file-name
       (file-name-directory
        (file-name-sans-extension (buffer-file-name)))))))

(defmacro nvp-indent-cl (fn)
  "Generally doesn't work."
  `(put ,fn 'lisp-indent-function 'common-lisp-indent-function))

(defmacro nvp-region-or-batp (&optional thing)
  "Region bounds if active or bounds of THING at point."
  `(if (region-active-p) (car (region-bounds))
     (bounds-of-thing-at-point ,(or thing ''symbol))))

;; -------------------------------------------------------------------
;;; Syntax

(defmacro nvp-within-bounds-of-thing-or-region (thing beg end &rest body)
  "Execute BODY with region widened to bounds of THING at point \
unless region is active.
BEG and END are bound to the bounds."
  (declare (indent defun) (debug (sexp sexp sexp &rest form)))
  `(if (not (region-active-p))
       (save-restriction
         (widen)
         (cl-destructuring-bind (,beg . ,end) (bounds-of-thing-at-point ,thing)
           ,@body))
     ,@body))

(defmacro nvp-unless-in-comment-or-string (&rest body)
  "Execute BODY unless currently in a string or comment."
  (declare (indent defun))
  `(unless (let ((ppss (syntax-ppss)))
             (or (car (setq ppss (nthcdr 3 ppss)))
                 (car (setq ppss (cdr ppss)))
                 (nth 3 ppss)))
     ,@body))

(defmacro nvp-unless-in-comment (&rest body)
  "Execute BODY unless in comment."
  (declare (indent defun) (debug t))
  `(unless (nth 4 (syntax-ppss))
     ,@body))

(defmacro nvp-unless-in-string (&rest body)
  "Execute BODY unless in string."
  (declare (indent defun))
  `(unless (nth 3 (syntax-ppss))
     ,@body))

;; -------------------------------------------------------------------
;;; Conversions

(defmacro nvp-listify (args)
  "Ensure ARGS is a list."
  (let ((args (if (stringp args) (intern args) args)))
    `(unless (consp ,args) (setq ,args (cons ,args nil)))))

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
;;; Regex / Strings

(defmacro nvp-search-and-go (regexp &optional back &rest body)
  "Search forward or backward for REGEXP, and move point to beginning of 
line at match (default) or do BODY at point if non-nil."
  `(let ((start (point)))
     (condition-case nil
         (progn
           (forward-line ,(if back -1 1))
           (,(if back 're-search-backward 're-search-forward) ,regexp)
           ,@(or body (list '(beginning-of-line))))
       (error (goto-char start)))))

(defmacro nvp-re-opt (opts &optional no-symbol)
  `(eval-when-compile
     (concat ,(and (not no-symbol) "\\_<") (regexp-opt ,opts t)
             ,(and (not no-symbol) "\\_>"))))

(defmacro nvp-concat (&rest body)
  `(eval-when-compile (concat ,@body)))

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
;;; Programs / Paths
(declare-function nvp-setup-program "nvp-setup")

(defmacro nvp-w32-program (name)
  "Name of cached program on shitty w32.e"
  (and (symbolp name) (setq name (symbol-name name)))
  `(intern (concat "nvp-" ,name "-program")))

;; PATH can contain environment variables to expand
;; if NO-COMPILE is defined the name is determined at runtime
(defmacro nvp-program (name &optional no-compile path)
  (let* ((name (cond
                 ((symbolp name) (symbol-name name))
                 ((consp name)
                  (pcase name
                    (`(quote ,sym)
                      (symbol-name sym))
                    (_ name)))
                 ((stringp name) name)
                 (t (user-error "%S unmatched"))))
         (path (and path (substitute-env-in-file-name path))))
    `(,(if no-compile 'progn 'eval-when-compile)
      (declare-function nvp-setup-program "nvp-setup")
      (or (nvp-with-gnu/w32
               (cl-loop for p in
                   (delq nil (cons ,path '("~/bin/" "~/.asdf/shims" "~/.local/bin/"
                                           "/usr/local/bin")))
                 do (let ((f (expand-file-name ,name p)))
                      (and (file-exists-p f)
                           (file-executable-p f)
                           (cl-return f))))
            (bound-and-true-p (intern (concat "nvp-" ,name "-program"))))
          (executable-find ,name)
          ;; fallback to runtime search
          (and (require 'nvp-setup)
               (nvp-setup-program ,name ,path))))))

(defmacro nvp-path (path &optional no-compile)
  `(,(if no-compile 'progn 'eval-when-compile)
     (let ((path (substitute-env-in-file-name ,path)))
       (and path (file-exists-p path) path))))

(defmacro nvp-mode-config-path (mode &optional ensure-string)
  "Create path for MODE config file."
  `(progn
     (expand-file-name
      (concat "nvp-" ,(if ensure-string (nvp-stringify mode) `,mode)
              "-config.el")
      nvp/config)))

(defmacro nvp-cache-file-path (filename)
  "Create cache path for FILENAME."
  `(progn (expand-file-name ,(nvp-stringify filename) nvp/cache)))

;; -------------------------------------------------------------------
;;; REPLs

(cl-defmacro nvp-hippie-shell-fn (name histfile
                                       &key
                                       (size 5000)
                                       (history ''comint-input-ring)
                                       (bol-fn ''comint-line-beginning-position)
                                       history-fn expand-fn)
  "Setup comint history ring read/write and hippie-expand for it."
  (let ((fn (nvp-string-or-symbol name)))
    `(progn
       (eval-when-compile
         (defvar comint-input-ring-file-name)
         (defvar comint-input-ring-size))
       (declare-function comint-read-input-ring "comint")
       (declare-function comint-write-input-ring "comint")
       (autoload 'hippie-expand-shell-setup "hippie-expand-shell")
       (defun ,fn ()
         (setq comint-input-ring-file-name (expand-file-name ,histfile nvp/cache))
         (setq comint-input-ring-size ,size)
         (comint-read-input-ring)
         (add-hook 'kill-buffer-hook 'comint-write-input-ring nil 'local)
         (hippie-expand-shell-setup
          :history ,history
          :bol-fn ,bol-fn
          :history-fn ,history-fn
          :expand-fn ,expand-fn)))))

;; switching between REPLs and source buffers -- maintain the name of the
;; source buffer as a property of the process running the REPL. Uses REPL-FIND-FN
;; if supplied to find/create the REPL buffer, REPL-LIVE-P is called to check
;; if it is alive (defaults to `buffer-live-p'
;; if REPL-HISTORY is non-nil `nvp-comint-add-history-sentinel' is added before the
;; buffers process-filter. REPL-INIT is called to create and return a new REPL
;; buffer. REPL-CONFIG is executed in the new REPL buffer after creation
(cl-defmacro nvp-repl-switch (name (&key repl-mode repl-buffer-name repl-find-fn
                                         repl-live-p repl-history repl-process
                                         repl-config repl-wait
                                         repl-doc (repl-switch-fn ''pop-to-buffer))
                              &rest repl-init)
  (declare (indent defun))
  (autoload 'nvp-comint-add-history-sentinel "nvp-comint")
  (let ((fn (intern (format "nvp-%s-repl-switch" name))))
    `(defun ,fn ()
       ,(or repl-doc "Switch between source and REPL buffers")
       (interactive)
       (if ,(or (and repl-mode `(eq major-mode ,repl-mode))
                (and repl-buffer-name `(equal ,repl-buffer-name (buffer-name))))
           ;; in REPL buffer, switch back to source
           (switch-to-buffer-other-window
            ;; switch to set source buffer or the most recent other buffer
            (or (process-get
                 ,(or repl-process '(current-buffer-process)) :src-buffer)
                (other-buffer (current-buffer) 'visible)))
         ;; in source buffer, try to go to a REPL
         (let ((src-buffer (current-buffer))
               (repl-buffer
                ;; Should there be a default?
                (or ,(and repl-buffer-name
                          `(get-buffer ,repl-buffer-name))
                    ,(and repl-find-fn
                          `(ignore-errors (funcall ,repl-find-fn))))))
           ;; there is a REPL buffer, but is it alive?
           (when (not (funcall ,(or repl-live-p ''comint-check-proc) repl-buffer))
             ;; no, so we need to start one somehow -- this should return the
             ;; buffer object
             (setq repl-buffer (progn ,@repl-init))
             ,@(and repl-wait `((sit-for ,repl-wait)))
             (and (processp repl-buffer)
                  (setq repl-buffer (process-buffer repl-buffer))
                  ;; add a sentinel to write comint histfile before other
                  ;; sentinels that may be set by the mode
                  ,(and repl-history
                        '(nvp-comint-add-history-sentinel))))
           ;; Now switch to REPL and set its properties to point back to the source
           ;; buffer from whence we came
           (if (not (funcall ,(or repl-live-p ''comint-check-proc) repl-buffer))
               (error (message "The REPL didnt start!!!")))
           ,@(when repl-switch-fn
               `((funcall ,repl-switch-fn repl-buffer)))
           ;; only config first time through
           (when (not (process-get ,(or repl-process
                                        '(get-buffer-process repl-buffer))
                                   :src-buffer))
             ,(and repl-config `(funcall ,repl-config)))
           (process-put ,(or repl-process
                             '(get-buffer-process repl-buffer))
                        :src-buffer src-buffer))))))

;; -------------------------------------------------------------------
;;; Bindings

(defconst nvp-binding-prefix "<f2>")
(defconst nvp-major-mode-prefix "m")       ;eg <f2> m
(defconst nvp-major-minor-mode-prefix "m") ;eg <f2> m m

(defconst nvp-bindings-table
  (let* ((nvp nvp-binding-prefix)
         (major (concat nvp " " nvp-major-mode-prefix)))
    `((align    -> ,nvp "a")
      (abbrev   -> ,nvp "a")
      (bookmark -> ,nvp "b")
      (code     -> ,nvp "c")
      (count    -> ,nvp "c")
      (debug    -> ,nvp "d")
      (env      -> ,nvp "e")
      (edit     -> ,nvp "e")
      (find     -> ,nvp "f")
      (git      -> ,nvp "g")
      (help     -> ,nvp "h")
      (install  -> ,nvp "i")
      (jump     -> "C-x j")
      (list     -> ,nvp "l")
      (open     -> ,nvp "o")
      (session  -> ,nvp "o")
      (package  -> ,nvp "p")
      (toggle   -> ,nvp "q")
      (insert   -> ,nvp "q")
      (sort     -> ,nvp "s")
      (theme    -> ,nvp "t")
      (tag      -> ,nvp "T")
      (vagrant  -> ,nvp "v")
      (web      -> ,nvp "w")
      (process  -> ,nvp "x")
      (shell    -> ,nvp "z")
      (repl     -> ,nvp "z")
      ;; major mode bindings
      (major    -> ,major
                (abbrev  -> "a")
                (compile -> "c")
                (debug   -> "d")
                (doc     -> "D")
                (env     -> "e")
                (file    -> "f")
                (help    -> "h")
                (install -> "i")
                (jump    -> "j")
                (load    -> "l")
                (minor   -> ,nvp-major-minor-mode-prefix)
                (package -> "p")
                (style   -> "s")
                (test    -> "t")
                (tag     -> "T")
                (process -> "x")
                (repl    -> "z"))
      ())))

;; create binding from table
(defun nvp--bind-lookup (sym &optional table)
  (or (cdr (cdr (assoc sym (or table nvp-bindings-table))))
      (error "%s not in bindings table" sym)))

(defun nvp--bind-concat (sym &optional table)
  (mapconcat 'identity (nvp--bind-lookup sym table) " "))

(defun nvp--bind (sym &optional key table)
  (pcase sym
    ('major
     (unless key
       (error "major mode binding requires subkey"))
     (let* ((major (nvp--bind-lookup 'major table))
            (val (nvp--bind-lookup key (cdr major))))
       (unless val
         (error "%s not found in bindings table" key))
       (mapconcat 'identity (cons (car major) val) " ")))
    ('minor
     (nvp--bind 'major 'minor table))
    (_ (nvp--bind-concat sym table))))

(defmacro nvp-define-key (keymap key category mode-type func)
  `(define-key ,keymap (kbd (nvp-bind ,key ,category ,mode-type)) ,func))

(defmacro nvp-bind (key category &optional mode-type)
  "Create binding for KEY in CATEGORY, optionally of MODE-TYPE which 
could be either 'major or 'minor."
  (cl-destructuring-bind (_ cat) category
    (mapconcat 'identity
               (delq nil
                     (list (pcase mode-type
                             (`'major (nvp--bind 'major cat))
                             (`'minor (nvp--bind 'minor))
                             (_ (nvp--bind cat)))
                           key))
               " ")))

(defmacro nvp-def-key (map key cmd)
  "Bind KEY, being either a string, vector, or keymap in MAP to CMD."
  (declare (debug t))
  (and (symbolp key) (setq key (symbol-value key)))
  (cl-assert (or (vectorp key) (stringp key) (keymapp key)))
  `(define-key
     ,(if (keymapp map) `',map map)
     ,(if (or (vectorp key) (keymapp key)) key (kbd key))
     ,(cond
       ((or (null cmd) (and (consp cmd)
                            (or (equal (cdr cmd) '(nil))
                                (equal (cddr cmd) '(nil)))))
        nil)
       ((consp cmd)
        (cond
         ((equal (car cmd) 'function) `,cmd)
         ((equal (car cmd) 'quote) `#',(cadr cmd))
         (t `,cmd)))
       (t `#',cmd))))

(defmacro nvp-create-keymaps (leader &rest maps)
  "Create submaps from LEADER map. Optionally give name of keymap for 
menu entry."
  (declare (indent defun))
  `(progn
     ,@(cl-loop for (key . args) in maps
          as map = (pop args)
          as name = (and args (pop args))
          collect `(progn
                     (eval-when-compile (declare-function ,map "")) ;compiler
                     (define-prefix-command ',map nil ,name)
                     (nvp-def-key ,leader ,key ',map)))))

(defmacro nvp-global-bindings (&rest bindings)
  "Add BINDINGS to global keymap."
  (declare (indent defun))
  (macroexp-progn
   (cl-loop for (k . b) in bindings
      collect `(nvp-def-key (current-global-map) ,k ,b))))

(cl-defmacro nvp-bind-keys (map &rest bindings &key pred-form &allow-other-keys)
  "Add BINDINGS to MAP.
If PRED-FORM is non-nil, evaluate PRED-FROM before binding keys."
  (declare (indent defun) (debug t))
  (while (keywordp (car bindings))
    (setq bindings (cdr (cdr bindings))))
  (let ((map (nvp--normalize-modemap map)))
    `(progn
       (eval-when-compile (defvar ,map))
       (,@(if pred-form `(when ,pred-form) '(progn))
        ,@(cl-loop for (k . b) in bindings
             collect `(nvp-def-key ,map ,k ,b))))))

(cl-defmacro nvp-with-temp-bindings ((&key (keep t) exit bindings)
                                     &rest body)
  "Execute BODY with BINDINGS set in transient map."
  (declare (indent 0))
  (let ((tmap (cl-gensym)))
    `(let ((,tmap (make-sparse-keymap)))
       ,@(cl-loop for (k . b) in bindings
            collect `(nvp-def-key ,tmap ,k ,b))
       (set-transient-map ,tmap ,keep ,exit)
       ,@body)))

(cl-defmacro nvp-bindings (mode &optional feature &rest bindings
                                &key local minor &allow-other-keys)
  "Set MODE BINDINGS after FEATURE is loaded.
If LOCAL is non-nil, make map buffer local."
  (declare (indent defun))
  (while (keywordp (car bindings))
    (setq bindings (cdr (cdr bindings))))
  (let ((modemap (nvp--normalize-modemap mode minor)))
    `(progn
       (eval-when-compile (defvar ,modemap))
       ,(when local
          `(make-local-variable ',modemap))
       (with-eval-after-load ,(or feature `',(intern mode))
         ,@(cl-loop for (k . b) in bindings
              collect `(nvp-def-key ,modemap ,k ,b))))))

(cl-defmacro nvp-bindings-multiple-modes (modes &rest bindings &allow-other-keys)
  "Add shared BINDINGS to multiple MODES keymaps.
MODES is of the form ((mode-name . feature) ...).
Optional :local key can be set to make the mappings buffer-local."
  (declare (indent 1))
  `(progn
     ,@(cl-loop for (mode . feature) in modes
          collect `(nvp-bindings ,mode ',feature ,@bindings))))

;; -------------------------------------------------------------------
;;; Bindings: local / transient / overriding

(cl-defmacro nvp-use-transient-bindings (bindings
                                         &key (keep t)
                                         (pre '(nvp-indicate-cursor-pre))         
                                         (exit '(nvp-indicate-cursor-post))
                                         &allow-other-keys)
  "Set BINDINGS in transient map.
Run PRE form prior to setting commands."
  (declare (indent 0))
  (while (keywordp (car bindings))
    (setq bindings (cdr (cdr bindings))))
  (macroexp-let2* nil ((pre pre) (exit exit))
   `(progn
      (nvp-declare "nvp-indicate" nvp-indicate-cursor-pre nvp-indicate-cursor-post)
      ,pre
      (set-transient-map
       (let ((tmap (make-sparse-keymap)))
         ,@(cl-loop for (k . b) in bindings
              collect `(nvp-def-key tmap ,k ,b))
         tmap)
       ,keep
       ,exit))))

(cl-defmacro nvp-use-local-bindings (&rest bindings &key buffer &allow-other-keys)
  "Set local BINDINGS.
If BUFFER is non-nil, set local bindings in BUFFER."
  (declare (indent defun))
  (while (keywordp (car bindings))
    (setq bindings (cdr (cdr bindings))))
  `(let ((lmap (make-sparse-keymap)))
     (set-keymap-parent lmap (current-local-map))
     ,@(cl-loop for (k . b) in bindings
          collect `(nvp-def-key lmap ,k ,b))
     ,(if buffer `(with-current-buffer ,buffer (use-local-map lmap))
        `(use-local-map lmap))))

;; Overrides a minor mode keybinding for the local buffer by creating
;; or altering keymaps stored in buffer-local variable 
;; `minor-mode-overriding-map-alist'.
(defmacro nvp-use-minor-mode-overriding-map (mode bindings)
  "Override minor MODE BINDINGS using `minor-mode-overriding-map-alist'."
  (declare (indent defun))
  `(let ((map (make-sparse-keymap)))
     (macroexp-progn
      (cl-loop for (k . b) in ,bindings
         collect `(nvp-def-key ,map ,k ,b)))
     (push (cons ,mode map) minor-mode-overriding-map-alist)))

(defmacro nvp-use-local-keymap (keymap &rest bindings)
  "Use a local version of keymap."
  (declare (indent defun))
  `(progn
     (make-local-variable ',keymap)
     (let ((newmap (make-sparse-keymap)))
       (set-keymap-parent newmap ,keymap)
       ,@(cl-loop for (k . b) in bindings
            collect `(nvp-def-key newmap ,k ,b))
       (set ',keymap newmap))))

;; -------------------------------------------------------------------
;;; Bindings: view 

;; general movement bindings for non-insert modes
(defmacro nvp-bindings-view ()
  ''(("j"     . next-line) ;; use instead of forward-line since it is often advised
     ("k"     . previous-line)
     ("h"     . backward-char)
     ("l"     . forward-char)
     ("e"     . end-of-line)
     ("a"     . beginning-of-line)
     ("A"     . beginning-of-buffer)
     ("E"     . end-of-buffer)
     ("/"     . isearch-forward)
     ("?"     . isearch-backward)
     ("SPC"   . scroll-down)
     ("S-SPC" . scroll-up)
     ("M-n"   . nil)
     ("M-p"   . nil)
     ("M-s-n" . nvp-move-forward-heading)
     ("M-s-p" . nvp-move-previous-heading)
     ("M-N"   . nvp-move-forward-paragraph)
     ("M-P"   . nvp-move-backward-paragraph)))

(defalias 'nvp-bindings-with-view 'nvp-bindings-modal-view)
(defmacro nvp-bindings-modal-view (mode &optional feature &rest bindings)
  (declare (indent defun))
  (let ((bs `(,@(nvp-bindings-view) ,@bindings)))
    `(nvp-bindings ,mode ,feature ,@bs)))

(defmacro nvp-bindings-add-view (mode &optional feature)
  `(progn (nvp-bindings ,mode ,feature ,@(nvp-bindings-view))))

(defmacro nvp-with-view-bindings (&rest body)
  `(nvp-with-temp-bindings
     (:bindings ,@(nvp-bindings-view)
                :keep t
                :exit (lambda () (message "vim out")))
     ,@body))

;; -------------------------------------------------------------------
;;; Files

(defmacro nvp-file-same (file-1 file-2)
  "Return non-nil if FILE-1 and FILE-2 are the same."
  (declare (indent defun))
  `(when (and (file-exists-p ,file-1) (file-exists-p ,file-2))
     (equal (file-truename ,file-1) (file-truename ,file-2))))

;; -------------------------------------------------------------------
;;; Buffers / IO

;; modified from smartparens.el
(defmacro nvp-with-buffers-using-mode (mode &rest body)
  "Execute BODY in every existing buffer using `major-mode' MODE."
  (declare (indent 1))
  `(dolist (buff (buffer-list))
     (when (provided-mode-derived-p ,mode (buffer-local-value 'major-mode buff))
       (with-current-buffer buff
         ,@body))))

(defmacro nvp-last-input-char ()
  "Return the last character input."
  '(kbd (substring (edmacro-format-keys (vector last-input-event)) -1)))

;; read input in various ways
(defmacro nvp-read (prompt &optional thing &rest args)
  (declare (indent defun))
  (pcase thing
    ((pred stringp)
     `(read-from-minibuffer ,prompt ,thing))
    (`(quote ,sym)
      (cond
        ((consp sym)
         `(ido-completing-read ,prompt ,thing))
        ((vectorp sym)
         `(completing-read ,prompt ,thing))
        ((symbol-function sym)
         (if args
             `(funcall-interactively ',sym ,@args)
           `(call-interactively ',sym)))
        (t `(ido-completing-read ,prompt ,sym))))
    ((pred symbolp)
     (cond
       ((string= ":library" (symbol-name thing))
        `(completing-read ,prompt
                          (apply-partially
                           'locate-file-completion-table
                           load-path (get-load-suffixes))))
       ((vectorp (symbol-value (intern-soft thing)))
        `(completing-read ,prompt ,thing))
       (t `(ido-completing-read ,prompt (symbol-value ,thing)))))
    ((pred consp)
     `(ido-completing-read ,prompt ,thing))
    (_ `(read-from-minibuffer ,prompt))))

(defmacro nvp-with-results-buffer (&optional buffer-or-name &rest body)
  "Do BODY in temp BUFFER-OR-NAME as with `with-temp-buffer-window'.
Make the temp buffer scrollable, in `view-mode' and kill when finished."
  (declare (indent defun))
  `(let (other-window-scroll-buffer)
     (with-temp-buffer-window
      ,(or buffer-or-name "*results*")
      t
      nil
      (with-current-buffer standard-output
        (setq other-window-scroll-buffer (current-buffer))
        ,@body
        (view-mode-enter nil 'kill-buffer)))))

;; -------------------------------------------------------------------
;;; Time

(defmacro nvp-file-older-than-days (file days)
  "non-nil if FILE last modification was more than DAYS ago."
  (declare (indent defun) (debug t))
  `(< (time-to-seconds
       (time-subtract (current-time)
                      (nth 5 (file-attributes ,file))))
      (* 60 60 24 ,days)))

;; Measure and return the running time of the code block.
;; https://github.com/skeeto/.emacs.d/blob/master/lisp/extras.el#L83
(defmacro nvp-measure-time (&rest body)
  (declare (indent defun))
  (garbage-collect)
  (let ((start (make-symbol "start")))
   `(let ((,start (float-time)))
      ,@body
      (- (float-time) ,start))))

;; -------------------------------------------------------------------
;;; Tooltips

(declare-function pos-tip-show "pos-tip")

;;; TODO:
;; - use help buffer with xref?
;; - truncate popup
(cl-defmacro nvp-with-toggled-tip (popup
                                   &key
                                   (help-key "h")  ;key-binding for help-fn
                                   (help-fn t)     ;more help (t is default)
                                   bindings        ;additional bindings
                                   (timeout 45)    ;pos-tip timeout
                                   keep            ;keep transient map
                                   use-gtk         ;use gtk tooltips
                                   (help-buffer '(help-buffer)))
  "Toggle POPUP, a help string, in pos-tip. 
If HELP-FN is :none, HELP-KEY is not bound by default. 
Normally, HELP-KEY triggers a function to jump to a full help description 
related to the popup - hopefully in a buffer.
BINDINGS are an alist of (key . function) of additional keys to bind in the
transient keymap.
TIMEOUT is passed to `pos-tip-show'.
If USE-GTK is non-nil use gtk tooltips.
KEEP is passed to `set-transient-map'.
HELP-BUFFER is buffer with full help documentation. This is only applicable to the
default help function."
  (declare (indent defun) (debug t))
  (macroexp-let2* nil ((str popup)
                       (h-key (or help-key "h"))
                       (h-fn (cond
                              ((eq :none help-fn) nil) ;back-compat
                              ((eq t help-fn)
                               `(lambda ()
                                  (interactive)
                                  (x-hide-tip)
                                  (with-help-window ,help-buffer
                                    (with-current-buffer standard-output
                                      (insert ,str)))
                                  ;; (with-current-buffer ,help-buffer
                                  ;;   (insert ,str)
                                  ;;   (view-mode-enter)
                                  ;;   (pop-to-buffer (current-buffer)))
                                  ))
                              (help-fn help-fn)
                              (t nil)))
                       (exit-fn '(lambda () (interactive) (x-hide-tip)))
                       (keep keep))
    `(progn
       (declare-function pos-tip-show "pos-tip")
       (let ((x-gtk-use-system-tooltips ,use-gtk))
         (unless (x-hide-tip)
           (pos-tip-show ,str nil nil nil ,timeout)
           (set-transient-map
            (let ((tmap (make-sparse-keymap)))
              (define-key tmap ,h-key ,h-fn)
              ,@(cl-loop for (k . b) in bindings
                   collect `(define-key tmap ,k ,b))
              tmap)
            ,keep
            ,exit-fn))))))

;; -------------------------------------------------------------------
;;; Processes

;; get a comint buffer, run body, return buffer
(defmacro nvp-comint-buffer (name &rest body)
  (declare (indent 2) (indent 1))
  `(progn (with-current-buffer (generate-new-buffer-name ,name)
            (comint-mode)
            ,@body
            (current-buffer))))

(defmacro nvp-with-comint-buffer (name &rest body)
  (declare (indent defun))
  `(nvp-comint-buffer ,name ,@body))

(defmacro nvp-process-buffer (&optional name new-buff comint &rest body)
  "Return a process buffer name NAME.
If NEW-BUFF generate a new buffer.  Optionally initialize buffer in
COMINT mode or with BODY."
  (declare (indent 2) (indent 1))
  (let ((get-buffer-function (if new-buff 'generate-new-buffer
                               'get-buffer-create))
        (name (or name "*nvp-install*")))
    (if (not (or comint body))
        `(,get-buffer-function ,name)
      `(progn (with-current-buffer (,get-buffer-function ,name)
                ,@(or body (list '(comint-mode)))
                (current-buffer))))))

(defmacro nvp-with-process-filter (process &optional proc-filter)
  "Run processs with `nvp-process-buffer-filter'.
Return process object."
  (declare (indent defun))
  (if (and proc-filter (eql proc-filter :none)) `,process
    (and (not proc-filter) (setq proc-filter ''nvp-process-buffer-filter))
    (macroexp-let2* nil ((process process) (proc-filter proc-filter))
      `(prog1 ,process
         (set-process-filter ,process ,proc-filter)))))

(defmacro nvp-with-sentinel (&optional on-error &rest body)
  "With process sentinel do ON-ERROR if exist status isn't 0 or BODY with\
successful process exit buffer."
  (declare (indent 2) (indent 1) (debug t))
  `(function
    (lambda (p m)
     (nvp-log "%s: %s" nil (process-name p) m)
     (if (not (zerop (process-exit-status p)))
         ,(or on-error '(pop-to-buffer (process-buffer p)))
       ,@body))))

(cl-defmacro nvp-with-process-log (process &key
                                           on-error
                                           on-success
                                           proc-filter
                                           (display-action t))
  "Log output in log buffer, if on-error is :pop-on-error, pop to log
if process exit status isn't 0."
  (declare (indent defun))
  (macroexp-let2* nil ((proc `(nvp-with-process-filter ,process ,proc-filter))
                       (on-err (if (and (symbolp on-error)
                                        (equal on-error :pop-on-error))
                                   `(pop-to-buffer (process-buffer ,proc)
                                                   ,display-action)
                                 on-error)))
    `(progn
       (set-process-sentinel ,proc
                             #'(lambda (p m)
                                 (nvp-log "%s: %s" nil (process-name p) m)
                                 (if (zerop (process-exit-status p))
                                     ,on-success
                                   ,on-error)))
       (display-buffer (process-buffer ,proc) ,display-action))))

(cl-defmacro nvp-with-process-buffer (process &key on-error on-success proc-filter)
  "Log PROCESS output in log buffer, do ON-ERROR and ON-SUCCESS in process buffer."
  (declare (indent defun))
  `(set-process-sentinel (nvp-with-process-filter ,process ,proc-filter)
    #'(lambda (p m)
        (nvp-log "%s: %s" nil (process-name p) m)
        (with-current-buffer (process-buffer p)
          (if (zerop (process-exit-status p))
              ,@on-success
            ,@on-error)))))

(cl-defmacro nvp-with-process (process
                               &key
                               (proc-name process)
                               (proc-buff `,(concat "*" proc-name "*"))
                               (proc-args nil)
                               (proc-filter t)
                               (buffer-fn 'get-buffer-create)
                               (on-success `(progn
                                              (nvp-indicate-modeline-success
                                               ,(concat " " proc-name " success"))
                                              (kill-buffer (current-buffer))))
                               (on-failure '(pop-to-buffer (current-buffer))))
  "Start PROCESS with a sentinel doing ON-SUCCESS or ON-FAILURE in process buffer."
  (declare (indent defun) (debug t))
  (let ((proc (make-symbol "proc")))
    `(progn
       (declare-function nvp-indicate-modeline-success "nvp-indicate")
       (declare-function nvp-log "nvp-log")
       (let ((,proc (start-process
                     ,(or proc-name process) (,buffer-fn ,proc-buff)
                     ,process ,@proc-args)))
         ,(cond
           ((eq proc-filter t)
            `(set-process-filter ,proc 'nvp-process-buffer-filter))
           (proc-filter
            `(set-process-filter ,proc ,proc-filter))
           (t nil))
         (set-process-sentinel ,proc
                               (lambda (p m)
                                 (nvp-log "%s: %s" nil (process-name p) m)
                                 (with-current-buffer (process-buffer p)
                                   (if (zerop (process-exit-status p))
                                       ,on-success
                                     ,on-failure))))))))

(defmacro nvp-with-process-wrapper (wrapper &rest body)
  "Wrap `set-process-sentinel' to so BODY is executed in environment
where WRAPPER has effect, eg. `cl-letf' will have effect. 
Note: use lexical-binding."
  (let ((sps (cl-gensym))
        (proc (cl-gensym))
        (fn (cl-gensym)))
    (macroexp-let2 nil wrapper wrapper
     `(let ((,sps (symbol-function 'set-process-sentinel)))
        (cl-letf (((symbol-function 'set-process-sentinel))
                  (lambda (,proc ,fn)
                    (funcall ,sps ,proc (funcall wrapper ,fn))))
          ,@body)))))

(defmacro nvp-with-async-override (orig-fn new-fn &rest body)
  "Set `symbol-function' of ORIG-FN to NEW-FN in process-buffer of 
BODY."
  (declare (indent defun))
  (macroexp-let2 nil new-fn new-fn
   `(with-sentinel-wrapper
     (lambda (fn)
       (let ((fun fn))
         (lambda (p m)
           (cl-letf (((symbol-function ,orig-fn) new-func))
             (funcall fun p m)))))
     ,@body)))

;; -------------------------------------------------------------------
;;; Installation wrappers: obsolete

(defmacro nvp-install--script (directory)
  "Find installation script."
  `(cl-loop for dir in '("script" "tools")
      for dirname = (expand-file-name dir ,directory)
      when (file-exists-p dirname)
      return (cl-loop for file in '("install.sh" "install")
                for fname = (expand-file-name file dirname)
                if (file-exists-p fname)
                return fname)))

(make-obsolete 'nvp-install 'nvp-with-install-script "")
(defmacro nvp-with-install-script (dir &optional funcs sudo &rest body)
  "Run installation script."
  (declare (indent defun) (debug defun))
  `(progn
     (require 'nvp)
     (require 'nvp-log)
     (require 'nvp-ext)
     (declare-function nvp-ext-run-script "nvp-ext")
     (declare-function nvp-log "nvp-log")
     (let ((script (nvp-install--script ,dir)))
       (nvp-with-process-log
         (funcall-interactively #'nvp-ext-run-script script
                                ,(if (stringp funcs) `(cons ,funcs nil)
                                   funcs)
                                ,sudo)
         :pop-on-error
         ,@body))))

(make-obsolete 'nvp-install 'nvp-with-script "")
(defmacro nvp-with-script (script &optional funcs sudo &rest body)
  "Run FUNCS in SCRIPT."
  (declare (indent defun) (debug defun))
  `(progn
     (require 'nvp)
     (require 'nvp-log)
     (require 'nvp-ext)
     (declare-function nvp-log "nvp-log")
     (nvp-with-process-log
       (funcall-interactively #'nvp-ext-run-script ,script
                              ,(if (stringp funcs) `(cons ,funcs nil)
                                 funcs)
                              ,sudo)
       :pop-on-error
       ,@body)))

(make-obsolete 'nvp-install 'nvp-with-asdf-install "")
(defmacro nvp-with-asdf-install (prefix dir plugin
                                        &optional config-defaults error-callback
                                        success-callback script-fn sudo
                                        &rest body)
  "Run install script, with prefix prompt for extra arguments to configure
and install PLUGIN with asdf."
  `(progn
     (require 'asdf)
     (declare-function asdf--versions "asdf")
     (declare-function asdf-install "asdf")
     (if ,prefix
         (let ((ver (ido-completing-read
                     ,(concat (capitalize plugin) " version: ")
                     (asdf--versions ,plugin)))
               (process-environment
                (cons (concat
                       ,(concat (upcase plugin) "_EXTRA_CONFIGURE_OPTIONS=")
                       (read-from-minibuffer
                        ,(concat (capitalize plugin) " configure options: ")
                        ,config-defaults))
                      process-environment)))
           (asdf-install ,plugin ver ,error-callback ,success-callback))
       (nvp-with-install-script ,dir ,(or script-fn "install") ,sudo ,@body))))

;; -------------------------------------------------------------------
;;; Building Interactive Functions

;;; Wrapper functions to call mode-local values
(defmacro nvp-wrapper-function (symbol &optional doc)
  "Creates a simple wrapper function to call local SYMBOL function with \
list of args.
Optionally use DOC for function."
  (let ((fn (intern (string-remove-suffix "-function" (symbol-name symbol))))
        (args (make-symbol "args")))
    `(defun ,fn (&rest ,args)
       ,(or doc (format "Function to run local `%s'." symbol))
       (interactive)
       (setq prefix-arg current-prefix-arg)
       (funcall-interactively ,symbol ,args))))

(defmacro nvp-wrapper-fuctions (&rest fun-docs)
  "Create list of wrapper functions.
FUN-DOCS is an alist of pairs of symbols with optional docs."
  (macroexp-progn
   (cl-loop for (sym . doc) in fun-docs
      collect `(nvp-wrapper-function ,sym ,doc))))

;; FIXME: most of these should either be generic or act on local variables
;; instead of being defined many times
;; Marks
(defmacro nvp-mark-defun (&optional first-time &rest rest)
  "Mark blocks, expanding successively."
  `(if (or (and (eq last-command this-command) (mark t))
           (and transient-mark-mode mark-active))
       (set-mark
        (save-excursion
          (goto-char (mark))
          ,@(or rest
                (list
                 '(smie-forward-sexp 'halfsexp)
                 '(point)))))
     ,(or first-time '(mark-defun))))

;;; Newline

;; Newline and indents for PAIRS, extends comment region with
;; COMMENT-START when inside COMMENT-RE.
(cl-defmacro nvp-newline (name &optional description
                               &key pairs comment-re comment-start)
  (declare (indent defun))
  (let ((fn (if (symbolp name) name (intern name)))
        (conds
         (cons 'or
               (cl-loop
                  for (open close) in pairs
                  collect `(and (looking-back ,open (line-beginning-position))
                                (looking-at ,close)))))
        (start-re (when comment-re (car comment-re)))
        (end-re (when comment-re (cdr comment-re))))
    `(defun ,fn ()
       ,(or description "Newline dwim.")
       (interactive)
       (let (,@(when pairs `((p ,conds)))
             ,@(when comment-re '((ppss (syntax-ppss)))))
         (cond
          ,@(when comment-re
              `(((and (nth 4 ppss)
                      (save-excursion
                        (forward-line 0)
                        (looking-at-p ,start-re)))
                 ,(when end-re
                    `(when (save-excursion
                             (end-of-line)
                             (looking-back ,end-re (line-beginning-position)))
                       (save-excursion
                         (newline-and-indent))))
                 (newline)
                 (insert ,comment-start)
                 (indent-according-to-mode))))
          (t
           (newline)
           ,@(when pairs
               '((when p
                   (save-excursion
                     (newline-and-indent)))))
           (indent-according-to-mode)))))))

;;; Compile

;; Create compile function, check for makefiles/cmake first, otherwise
;; execute BODY. Prefix argument executes PROMPT-ACTION, and its
;; result is bound to ARGS, which can be used in the body.
(cl-defmacro nvp-make-or-compile-fn
    (name 
     (&key
      (doc "Compile using make or cmake if found, otherwise execute body.")
      (make-action
       '(let ((compile-command (or args "make -k")))
          (nvp-compile)))
      (cmake-action
       '(nvp-compile-cmake args))
      (default-prompt
        '(read-from-minibuffer "Compile args: "))
      (prompt-action
       `((cond
          ,@(and cmake-action
                 '((have-cmake
                    (read-from-minibuffer "CMake args: "))))
          ,@(and make-action
                 '((have-make
                    (format "make %s" (read-from-minibuffer "Make args: ")))))
          (t ,default-prompt)))))
     &rest body)
  (declare (indent defun))
  (let ((fn (if (symbolp name) name (intern name))))
    `(defun ,fn (&optional arg)
       ,doc
       (interactive "P")
       (let* (,@(and make-action
                     '((have-make
                        (memq t (mapcar #'file-exists-p
                                        '("Makefile" "makefile" "GNUMakefile"))))))
              ,@(and cmake-action
                     '((have-cmake (file-exists-p "CMakeLists.txt"))))
              (args (and arg ,@(or prompt-action
                                   '((read-from-minibuffer "Compile args: "))))))
         (cond
          ,@(and cmake-action `((have-cmake ,cmake-action)))
          ,@(and make-action `((have-make ,make-action)))
          (t ,@body))))))

;;; Align

;; Create alignment functions
(defmacro nvp-align-fn (name doc regex &optional _ignore-string)
  (declare (indent defun))
  (let ((fn (intern (if (symbolp name)
                        (symbol-name name)
                      name))))
    `(progn
       ,(and (buffer-file-name)
             `(autoload ',name ,(buffer-file-name)))
       (defun ,fn (start end)
         ,doc
         (interactive "r")
         (align-regexp start end ,regex)))))

;;; Wrap

;; Create function to wrap region, inserting BEGIN at beginning,
;; AFTER at the end.
(defmacro nvp-wrap-fn (name doc begin end &optional interact-p)
  (declare (indent defun))
  (let ((fn (intern (if (symbolp name)
                        (symbol-name name)
                      name))))
    `(defun ,fn (start end)
       ,doc
       ,@(when interact-p
           '(interactive "r"))
       (save-excursion
         (goto-char (region-beginning))
         (insert ,begin))
       (goto-char (region-end))
       (insert ,end))))


;; Wrap items in list between DELIM, default wrapping with WRAP
;; Create list wrapping functions, wrapping items between DELIMS with
;; WRAP by default, prompting for wrapping string with prefix.  IGNORE
;; is regexp to ignore in list, ie commas and spaces and MATCH is regex
;; to capture items.
(cl-defmacro nvp-wrap-list-items (name
                                  &key
                                  (delims '("(" . ")"))
                                  (match "[^)(, \n\t\r]+")
                                  (ignore "[, \n\t\r]*")
                                  (wrap '("\"" . "\"")))
  (declare (debug defun)
           (indent defun))
  (let ((fn (intern (concat "nvp-list-wrap-" (symbol-name name))))
        (doc (format
              (concat "Wrap items of list in selected region between "
                      "%s...%s with items with %s..%s by default or "
                      "string ARG, prompting with prefix.")
              (car delims) (cdr delims) (car wrap) (cdr wrap)))
        (delim-re (concat ignore "\\(" match "\\)")))
    `(defun ,fn (start end &optional arg)
       ,doc
       (interactive "r\nP")
       (let* ((wrap (or (and arg
                             (car
                              (read-from-string
                               (read-from-minibuffer
                                "Wrap items with(a . b): "))))
                        ',wrap))
              (str (buffer-substring-no-properties start end)))
         (delete-region start end)
         (insert
          (with-temp-buffer
            (insert str)
            (goto-char (point-min))
            (re-search-forward ,(regexp-quote (car delims)) nil t)
            (while (re-search-forward ,delim-re nil t)
              (replace-match (concat (car wrap)
                                     (match-string-no-properties 1)
                                     (cdr wrap))
                             t nil nil 1))
            (buffer-string)))))))

;; -------------------------------------------------------------------
;;; URL

(defmacro nvp-path-to-uri (path)
  "Convert PATH to URI."
  `(url-hexify-string
    (concat "file://" (nvp-with-w32 "/") (file-truename ,path))
    url-path-allowed-chars))

(defmacro nvp-uri-to-path (uri)
  "Convert URI to file path."
  `(progn
     (when (keywordp ,uri) (setq ,uri (substring (symbol-name ,uri) 1)))
     (let ((retval (url-filename (url-generic-parse-url (url-unhex-string ,uri)))))
       (nvp-with-gnu/w32 retval (substring retval 1)))))

(defmacro nvp-with-url-buffer (url &rest body)
  "Do BODY in buffer with contents from URL."
  (declare (indent defun)
           (debug (sexp &rest form)))
  `(with-current-buffer (url-retrieve-synchronously ,url)
     ,@body))

(defmacro nvp-while-scanning-url (url regex &rest body)
  "Do BODY in buffer with URL contents at position of REGEX."
  (declare (indent defun)
           (debug (sexp sexp &rest form)))
  `(nvp-with-url-buffer ,url
     (goto-char (point-min))
     (while (re-search-forward ,regex nil t)
       ,@body)
     (kill-buffer)))

;; -------------------------------------------------------------------
;;; Advice

(defmacro nvp-advise-commands (advice where funcs &optional props)
  "Apply ADVICE at location WHERE to funcs."
  (declare (indent 0))
  `(progn
     ,@(mapcar (lambda (fn) `(advice-add ',fn ,where ,advice ,props)) funcs)))

(defmacro nvp-remove-all-advice (funcs)
  "Remove all advice from list of FUNCS."
  (declare (indent 0))
  `(progn
     ,@(cl-loop for fn in funcs
          collect `(advice-mapc (lambda (advice _props) (advice-remove ',fn advice))
                                ',fn))))

(defmacro nvp-eldoc-function (func &optional init)
  "Set local eldoc function."
  `(progn
     (add-function :before-until (local 'eldoc-documentation-function) #',func)
     ,(when init '(eldoc-mode))))

;; -------------------------------------------------------------------
;;; Mode specific

;; Hydras
(defmacro nvp-hydra-set-property (hydra-name &rest props)
  "Apply PROPS to HYDRA-NAME after `hydra' is loaded.
PROPS defaults to setting :verbosity to 1."
  (declare (indent 1))
  (unless props (setq props (list :verbosity 1)))
  `(progn
     (declare-function hydra-set-property "hydra")
     (with-eval-after-load 'hydra
      ,@(cl-loop for (k v) on props by #'cddr
           collect `(hydra-set-property ,hydra-name ,k ,v)))))

;; Hideshow
(defmacro nvp-hs-blocks (start end)
  `(progn
     (setq hs-block-start-regexp ,start)
     (setq hs-block-end-regexp ,end)))

(defmacro nvp-with-hs-block (start end &rest body)
  "Do hideshow with START and END regexps."
  (declare (indent defun))
  `(progn
     (require 'hideshow)
     (unless hs-minor-mode (hs-minor-mode))
     (let ((hs-block-start-regexp ,start)
           (hs-block-end-regexp ,end))
       ,@(or body (list '(hs-toggle-hiding))))))

;; smartparens
(cl-defmacro nvp-sp-local-pairs (&rest pairs &key modes &allow-other-keys)
  (declare (indent defun) (debug defun))
  (while (keywordp (car pairs))
    (setq pairs (cdr (cdr pairs))))
  `(progn
     (eval-when-compile (require 'smartparens))
     (declare-function sp-local-pair "smartparens")
     ,(cond
       (modes
        `(sp-with-modes ,modes
           ,@pairs))
       ((equal 'quote (caar pairs)) `(sp-local-pair ,@pairs))
       (t
        (macroexp-progn
         (cl-loop for pair in pairs
            collect `(sp-local-pair ,@pair)))))))

;; -------------------------------------------------------------------
;;; Package

(defmacro nvp-load-file-name ()
  "Expand to the file's name."
  '(cond
    (load-in-progress load-file-name)
    ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
     byte-compile-current-file)
    (t (buffer-file-name))))

(defmacro nvp-package-root (&optional name)
  "Expand to the default package directory with default prefix or NAME."
  (let ((prefix
         (if name
             (if (symbolp name) (symbol-name name) name)
           (file-name-nondirectory
            (directory-file-name
             (file-name-directory (nvp-load-file-name)))))))
    (intern (concat prefix "--dir"))))

(cl-defmacro nvp-package-define-root (&key name snippets dirs after-load)
  "Define package root directory with default prefix as directory name or NAME.
If SNIPPETS is non-nil, setup snippet loading for directory.
If DIRS is non-nil it should be a list of variables to define as directories
relative to the project root directory as symbols 'prefix--dir'.
AFTER-LOAD is a form to execute after file is loaded during which the root
directory is bound to `root' and all `dirs' are let-bound to their symbols."
  (declare (indent 0) (debug t))
  (let* ((file (cond
                (load-in-progress load-file-name)
                ((and (boundp 'byte-compile-current-file)
                      byte-compile-current-file)
                 byte-compile-current-file)
                (t (buffer-file-name))))
         (root-val (file-name-directory file))
         (base (file-name-nondirectory (directory-file-name root-val)))
         (prefix (if name (if (symbolp name) (symbol-name name) name) base))
         (root (intern (concat prefix "--dir")))
         (dirs (mapcar (lambda (d)
                         (and (symbolp d) (setq d (symbol-name d)))
                         (list (intern d)
                               (intern (concat prefix "--" d))
                               (expand-file-name d root-val)))
                       dirs))
         (mappings (cons `(root ,root) (mapcar 'butlast dirs))))
    `(progn
       (eval-and-compile
         (defconst ,root ,root-val))
       ,(when snippets `(nvp-package-load-snippets ,root))
       ,(when dirs
          `(progn
             ,@(cl-loop for (_orig-sym dir-sym dir-val) in dirs
                  collect `(defconst ,dir-sym ,dir-val))))
       ,(when after-load
          `(with-eval-after-load ,file
             (let ,mappings
               ,after-load))))))

(defmacro nvp-package-load-snippets (dir)
  "Add packages snippet directory to `yas-snippet-dirs' after loading
`yasnippet'."
  `(progn
     (eval-when-compile (defvar yas-snippet-dirs))
     (declare-function yas-load-directory "yasnippet")
     (with-eval-after-load 'yasnippet
       (let ((snippet-dir (expand-file-name "snippets" ,dir))
             (dirs (or (and (consp yas-snippet-dirs) yas-snippet-dirs)
                       (cons yas-snippet-dirs ()))))
         (unless (member snippet-dir dirs)
           (setq yas-snippet-dirs (delq nil (cons snippet-dir dirs))))
         (yas-load-directory snippet-dir)))))

;; -------------------------------------------------------------------
;;; Setup / Build init

;; Find locations for init constants
(defun nvp--setup-normalize-locs (locs &optional defaults)
  "Ensure LOCS is a list.
If LOCS is nil, use DEFAULTS.  If it is a symbol/function (list) get its value(s)."
  (if (null locs)
      (or defaults (nvp-with-gnu/w32 '("~/") '("~/" "d:/" "c:/")))
    (if (and (consp locs) (functionp (car locs)))
        (list (eval locs))
      (and (not (listp locs)) (setq locs (cons locs nil)))
      (mapcar (lambda (l)
                (cond
                 ((consp l) l)
                 ((symbolp l) (symbol-value l))
                 (t l)))
              locs))))

(defun nvp--setup-find-loc (locs &optional places file)
  "Find first existing location in LOCS."
  (let ((locs (nvp--setup-normalize-locs locs nil))
        (places (nvp--setup-normalize-locs places)))
    (cl-loop for loc in locs
       return (cl-loop for place in places
                 as root = (if (symbolp place) (symbol-value place) place)
                 as loc-name = (expand-file-name loc root)
                 when (file-exists-p loc-name)
                 return (if file (directory-file-name loc-name)
                          (file-name-as-directory loc-name))))))

(defun nvp--setup-subdirs (root &optional ignored)
  (and (symbolp root) (setq root (symbol-value root)))
  (cl-remove-if
   (lambda (f)
     (or (not (file-directory-p f))
         (cl-member (file-name-nondirectory f) ignored :test 'string=)))
   (directory-files root t "^[^.]")))


(cl-defmacro nvp-add-to-alist (&rest items
                                     &key
                                     (alist 'auto-mode-alist)
                                     (test 'equal)
                                     &allow-other-keys)
  "Add ITEMS, a list of cons cells, to ALIST using TEST to check if item \
is already present."
  (declare (indent defun) (debug t))
  (while (keywordp (car items))
    (setq items (cdr (cdr items))))
  (macroexp-progn
   (cl-loop for (k . v) in items
      with lst = (if (consp alist) alist (symbol-value alist))
      unless (cl-member (cons k (quote v)) lst :test test)
      collect `(push (cons ,k ',v) ,alist))))

(defmacro nvp-setup-diminish (&rest modes)
  "Diminish MODES in modeline."
  (declare (indent 0))
  (macroexp-progn
   (cl-loop for (feat . mode) in modes
      collect `(eval-after-load ',feat '(diminish ',mode)))))

(defmacro nvp-setup-consts (&rest vars)
  "Define consts in init."
  (declare (indent 0) (debug t))
  (macroexp-progn
   (cl-loop for (v dir places file) in vars
      as loc = (nvp--setup-find-loc dir places file)
      do (eval `(defconst ,v ,loc)) ;so subsequent vars can use
      collect `(defconst ,v ,loc))))

(defmacro nvp-setup-load-files (&rest files)
  (declare (indent 0))
  (macroexp-progn
   (cl-loop for f in files
      collect `(load ,f 'noerror 'nomessage))))

(defmacro nvp-setup-load-paths (&rest paths)
  (declare (indent 0))
  (macroexp-progn
   (cl-loop for p in paths
      collect `(add-to-list 'load-path ,p))))

(defmacro nvp-setup-hooks (hook &rest modes)
  "Add HOOK to all MODES hooks."
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for mode in modes
      as mode-hook = (nvp--normalize-hook mode)
      collect `(add-hook ',mode-hook #',hook))))

(defmacro nvp-setup-add-subdir-load-paths (root &optional ignored)
  "Add subdirs under ROOT to `load-path', ingnoring those listed in IGNORED."
  (declare (indent defun))
  (let* ((ignored (or ignored '("." ".." "unused" "ignored" "old")))
         (dirs (nvp--setup-subdirs root ignored)))
    (macroexp-progn
     (cl-loop for d in dirs
        collect `(add-to-list 'load-path ,d)))))

;; -------------------------------------------------------------------
;;; Warnings / Compat / Variables

(unless (fboundp 'ignore-errors)
  (defmacro ignore-errors (&rest body)
    `(condition-case nil (progn ,@body) (error nil))))

(defmacro eieio-declare-slot (name)
  (cl-pushnew name eieio--known-slot-names) nil)

(defmacro nvp-declare (package &rest funcs)
  (declare (indent defun))
  (macroexp-progn
   (cl-loop for func in funcs
      collect `(declare-function ,func ,package))))

(defmacro nvp-autoload (package &rest funcs)
  (declare (indent defun))
  (macroexp-progn
   (cl-loop for func in funcs
      collect `(autoload ',func ,package))))

(defmacro nvp-local-vars ()
  '(progn
     (defvar nvp/abbrevs)
     (defvar nvp/auto)
     (defvar nvp/auto-site)
     (defvar nvp/devel)
     (defvar nvp/site)
     (defvar nvp/home)
     (defvar nvp/project)
     (defvar nvp/info)
     (defvar nvp/bin)
     (defvar nvp/binw)
     (defvar nvp/msys)
     (defvar nvp/cygwin)
     (defvar nvp/vms)
     (defvar nvp/git)
     (defvar nvp/test)
     (defvar nvp/lisp)
     (defvar nvp/config)
     (defvar nvp/defs)
     (defvar nvp/modedefs)
     (defvar nvp/custom)
     (defvar nvp/data)
     (defvar nvp/template)
     (defvar nvp/snippet)
     (defvar nvp/class)
     (defvar nvp/work)
     (defvar nvp/bookmark)
     (defvar nvp/cache)
     (defvar nvp/backup)
     (defvar nvp/org)
     (defvar nvp/books)
     (defvar nvp/install)
     (defvar nvp/private)))

(provide 'nvp-macro)
;;; nvp-macro.el ends here
