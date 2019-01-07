;;; nvp-macro ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
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
(eval-when-compile
  (defvar eieio--known-slot-names))

;; Doesn't work
;; (put 'require 'byte-hunk-handler 'byte-compile-file-form-require)
(when (functionp 'backtrace-frames)
  (when (assoc '(t byte-compile-file-form-require
                   ((require 'nvp-macro))
                   nil)
               (backtrace-frames))
    (message "Warning: package 'nvp-macro required at runtime")))

;; -------------------------------------------------------------------
;;; Misc

(defmacro current-buffer-process ()
  `(get-buffer-process (current-buffer)))

(defmacro eieio-declare-slot (name)
  (cl-pushnew name eieio--known-slot-names) nil)

(defmacro nvp-bfn (&optional or-buffer)
  "Short buffer file name"
  `(file-name-nondirectory
    (file-name-sans-extension ,(if or-buffer
                                   '(or (buffer-file-name) (buffer-name))
                                 '(buffer-file-name)))))

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

(defmacro nvp-unless-in-comment-or-string (&rest body)
  "Execute BODY unless currently in a string or comment."
  (declare (indent defun))
  `(unless (let ((ppss (syntax-ppss)))
             (or (car (setq ppss (nthcdr 3 ppss)))
                 (car (setq ppss (cdr ppss)))
                 (nth 3 ppss)))
     ,@body))

;;; Conversions

(defmacro nvp-listify (args)
  "If ARGS are string or symbol, output as a list."
  (cond
   ((stringp args)
    `(cons ,args nil))
   ((symbolp args)
    `(if (consp ,args) ,args (cons ,args nil)))
   (t args)))

(defmacro nvp-stringify (sym)
  (if (symbolp sym) `(symbol-name ,sym) `,sym))

(defmacro nvp-symbolify (sym)
  (if (symbolp sym) `,sym `(intern ,sym)))

(defmacro nvp-string-or-symbol (sym)
  "If SYM is string convert to symbol."
  `(if (stringp ,sym) (intern ,sym) ,sym))

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

;; code folding
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

;; -------------------------------------------------------------------
;;; Package

(defmacro nvp-package-dir (dir)
  "Package local directory"
  `(progn
     (defvar ,dir nil)
     (when load-file-name
       (setq ,dir (file-name-directory load-file-name)))))

(defmacro nvp-package-var (var &rest init)
  (declare (indent 1))
  `(progn
     (defvar ,var nil)
     (when load-file-name
       (setq ,var ,@init))))

(defmacro nvp-package-after-compile (&rest body)
  `(when load-file-name
     ,@body))

(defmacro nvp-package-load-snippets (dir)
  "Add packages snippet directory to `yas-snippet-dirs' after loading
`yasnippet'."
  `(progn
     (eval-when-compile (defvar yas-snippet-dirs))
     (declare-function yas-load-directory "yasnippet")
     (eval-after-load 'yasnippet
       '(let ((dir (expand-file-name "snippets" ,dir))
              (dirs (or (and (consp yas-snippet-dirs) yas-snippet-dirs)
                        (cons yas-snippet-dirs ()))))
          (unless (member dir dirs)
            (setq yas-snippet-dirs (delq nil (cons dir dirs))))
          (yas-load-directory dir)))))

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
      (or (nvp-with-gnu/w32
              (cl-loop for p in
                   (delq nil (cons ,path '("~/.local/bin/" "/usr/local/bin")))
                 do (let ((f (expand-file-name ,name p)))
                      (and (file-exists-p f)
                           (cl-return f))))
            (bound-and-true-p (intern (concat "nvp-" ,name "-program"))))
          (executable-find ,name)))))

(defmacro nvp-path (path &optional no-compile)
  `(,(if no-compile 'progn 'eval-when-compile)
    (let ((path (substitute-env-in-file-name ,path)))
      (and path (file-exists-p path) path))))

(defmacro nvp-mode (mode)
  `(expand-file-name
    (concat "nvp-" ,mode) (bound-and-true-p nvp/mode)))

;; -------------------------------------------------------------------
;;; REPLs

(defmacro nvp-hippie-shell-fn (name history &optional size ring bol-fn)
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
         (setq comint-input-ring-file-name
               (expand-file-name ,history nvp/cache))
         (setq comint-input-ring-size ,(or size 2000))
         (comint-read-input-ring)
         (add-hook 'kill-buffer-hook 'comint-write-input-ring 'local)
         (hippie-expand-shell-setup
          ,(or ring ''comint-input-ring)
          ,(or bol-fn ''comint-line-beginning-position))))))

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

(defmacro nvp-create-keymaps (leader &rest maps)
  "Create submaps from LEADER map. Optionally give name of keymap for 
menu entry."
  (declare (indent defun))
  `(progn
     ,@(cl-loop for (key . args) in maps
          as map = (pop args)
          as name = (and args (pop args))
          collect `(progn
                     (defvar ,map)      ;compiler warnings
                     (define-prefix-command ',map nil ,name)
                     (define-key ,leader (kbd ,key) ',map)))))

(defmacro nvp-bind-keys (map &rest bindings)
  "Bind keys to MAP"
  (declare (indent defun))
  `(progn
     (eval-when-compile (defvar ,map))
     ,@(cl-loop for (k . b) in bindings
          collect `(define-key ,map (kbd ,k) ',b))))

(defmacro nvp-define-key (keymap key category mode-type func)
  `(define-key ,keymap (kbd (nvp-bind ,key ,category ,mode-type)) ,func))

(defmacro nvp-bindings (mode &optional feature &rest bindings)
  (declare (indent defun))
  (let ((modemap (intern (concat mode "-map"))))
    `(eval-after-load ,(or feature `',(intern mode))
       '(progn
          (eval-when-compile (defvar ,modemap))
          ,@(cl-loop for (k . b) in bindings
               collect `(define-key ,modemap (kbd ,k) ',b))))))

;; do BODY with BINDINGS set in transient map
(cl-defmacro nvp-with-temp-bindings ((&key (keep t) exit bindings)
                                     &rest body)
  (declare (indent 0))
  (let ((tmap (cl-gensym)))
    `(let ((,tmap (make-sparse-keymap)))
       ,@(cl-loop for (k . b) in bindings
            collect `(define-key ,tmap (kbd ,k) ',b))
       (set-transient-map ,tmap ,keep ,exit)
       ,@body)))

;; FIXME: eval
(defmacro nvp-common-bindings (modes &rest bindings)
  (declare (indent defun))
  (macroexp-progn
   (cl-loop for mode in (eval modes)
      collect `(nvp-bindings ,mode ,@bindings))))

;;; general movement bindings for non-insert modes
(declare-function nvp-basic-up-paragraph "nvp-basic")
(declare-function nvp-basic-down-paragraph "nvp-basic")
(defmacro nvp-bindings-view ()
  ''(("j"    . next-line) ;; use instead of forward-line since it is often advised
     ("k"    . previous-line)
     ("h"    . backward-char)
     ("l"    . forward-char)
     ("e"    . end-of-line)
     ("a"    . beginning-of-line)
     ("A"    . beginning-of-buffer)
     ("E"    . end-of-buffer)
     ("/"    . isearch-forward)
     ("M-n"  . nil)
     ("M-p"  . nil)
     ("M-N"  . nvp-basic-down-paragraph)
     ("M-P"  . nvp-basic-up-paragraph)))

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

(defmacro nvp-with-local-bindings (&rest bindings)
  "Set buffer local bindings."
  (declare (indent defun))
  `(let ((oldmap (current-local-map))
         (newmap (make-sparse-keymap)))
     (when oldmap (set-keymap-parent newmap oldmap))
     ,@(cl-loop for (k . b) in bindings
          collect `(define-key newmap (kbd ,k) ',b))
     (use-local-map newmap)))

(defmacro nvp-with-local-keymap (keymap &rest bindings)
  "Use a local version of keymap."
  (declare (indent defun))
  `(progn
     (make-local-variable ',keymap)
     (let ((newmap (make-sparse-keymap)))
       (set-keymap-parent newmap ,keymap)
       ,@(cl-loop for (k . b) in bindings
            collect `(define-key newmap (kbd ,k) ',b))
       (set ',keymap newmap))))

;; -------------------------------------------------------------------
;;; Read / Input

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

(defmacro nvp-read-obarray (prompt &optional regexp)
  "Completing read for obarray with optional REGEXP filter."
  `(ido-completing-read
    ,prompt
    (let (r)
      (mapatoms
       (lambda (x)
         ,(if regexp
              `(when (string-match-p ,regexp (symbol-name x))
                 (push x r))
            `(push x r))))
      (mapcar 'symbol-name r))))

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
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,@body
       (- (float-time) ,start))))

;; -------------------------------------------------------------------
;;; Tooltips

(declare-function pos-tip-show "pos-tip")
(declare-function nvp-basic-temp-binding "nvp-basic")

(cl-defmacro nvp-with-toggled-tip (popup &key use-gtk help-fn bindings)
  "Toggle POPUP in pos-tip. If HELP-FN is :none, 'h' is not bound by default."
  (declare (indent defun))
  (let ((str (make-symbol "str")))
    `(progn
       (declare-function pos-tip-show "pos-tip")
       (declare-function nvp-basic-temp-binding "nvp-basic")
       (let ((x-gtk-use-system-tooltips ,use-gtk))
         (or (x-hide-tip)
             (let ((,str ,popup))
               (pos-tip-show ,str nil nil nil 20)
               (nvp-basic-temp-binding
                "h" ,(cond
                      ((eq :none help-fn) nil)
                      (help-fn help-fn)
                      (t `(lambda ()
                            (interactive)
                            (x-hide-tip)
                            (with-current-buffer (get-buffer-create "*nvp-help*")
                              (insert ,str)
                              (view-mode-enter)
                              (pop-to-buffer (current-buffer)))))))
               (nvp-basic-temp-binding
                "q" #'(lambda () (interactive) (let ((x-gtk-use-system-tooltips nil))
                                                 (x-hide-tip))))
               ,@(cl-loop for (k . b) in bindings
                    collect `(nvp-basic-temp-binding (kbd ,k) ,b))))))))

;; -------------------------------------------------------------------
;;; Processes

;; get a comint buffer, run body, return buffer
(defmacro nvp-comint-buffer (name &rest body)
  (declare (indent 2) (indent 1))
  `(progn (with-current-buffer (get-buffer-create ,name)
            (comint-mode)
            ,@body
            (current-buffer))))

(defmacro nvp-with-comint-buffer (name &rest body)
  (declare (indent defun))
  `(nvp-comint-buffer ,name ,@body))

(defmacro nvp-process-buffer (&optional comint &rest body)
  (declare (indent 2) (indent 1))
  (if (not (or comint body))
      '(get-buffer-create "*nvp-install*")
    `(progn (with-current-buffer (get-buffer-create "*nvp-install*")
              ,@(or body (list '(comint-mode)
                               '(current-buffer)))))))

(defmacro nvp-with-process-filter (process)
  "Run processs with `nvp-process-buffer-filter'. Return process object."
  (declare (indent defun))
  `(let ((proc ,process))
     (set-process-filter proc 'nvp-process-buffer-filter)
     proc))

(defmacro nvp-with-sentinel (&optional on-error &rest body)
  (declare (indent 2) (indent 1) (debug t))
  `(function
    (lambda (p m)
      (nvp-log "%s: %s" nil (process-name p) m)
      (if (not (zerop (process-exit-status p)))
          ,(or on-error '(pop-to-buffer (process-buffer p)))
        ,@body))))

(defmacro nvp-with-process-log (process &optional on-error &rest body)
  "Log output in log buffer, if on-error is :pop-on-error, pop to log
if process exit status isn't 0."
  (declare (indent 0))
  (let ((err (if (and (symbolp on-error)
                      (equal on-error :pop-on-error))
                 '(pop-to-buffer (nvp-process-buffer))
               on-error)))
    `(progn
       (set-process-sentinel
        (nvp-with-process-filter ,process)
        #'(lambda (p m)
            (nvp-log "%s: %s" nil (process-name p) m)
            (if (not (zerop (process-exit-status p)))
                ,err
              ,@body)))
       (display-buffer (nvp-process-buffer)))))

(defmacro nvp-with-process-buffer (process &optional on-error
                                           &rest body)
  "Log output in log buffer, do ON-ERROR and BODY in process buffer."
  (declare (indent defun))
  `(set-process-sentinel
    (nvp-with-process-filter ,process)
    #'(lambda (p m)
        (nvp-log "%s: %s" nil (process-name p) m)
        (with-current-buffer (process-buffer p)
          (if (not (zerop (process-exit-status p)))
              ,@on-error
            ,@body)))))

(cl-defmacro nvp-with-process (process
                               (&key
                                (on-success `(nvp-indicate-modeline-success
                                              ,(concat process " success")))
                                (proc-buff `,(concat "*" process "*"))
                                (proc-args nil))
                               &rest on-failure)
  "Start PROCESS with a sentinel doing ON-SUCCESS or ON-FAILURE."
  (declare (indent defun))
  `(set-process-sentinel
    (start-process
     ,process
     (with-current-buffer (get-buffer-create ,proc-buff)
       (setq buffer-read-only nil)
       (erase-buffer)
       (current-buffer))
     ,process ,@proc-args)
    #'(lambda (p m)
        (nvp-log "%s: %s" nil (process-name p) m)
        (if (zerop (process-exit-status p))
            ,on-success
          ,@on-failure))))

(defmacro nvp-with-process-wrapper (wrapper &rest body)
  "Wrap `set-process-sentinel' to so BODY is executed in environment
where WRAPPER has effect, eg. `cl-letf' will have effect. 
Note: use lexical-binding."
  (let ((sps (cl-gensym))
        (proc (cl-gensym))
        (fn (cl-gensym)))
    `(let ((,sps (symbol-function 'set-process-sentinel)))
       (cl-letf (((symbol-function 'set-process-sentinel))
                 (lambda (,proc ,fn)
                   (funcall ,sps ,proc (funcall ,wrapper ,fn))))
         ,@body))))

(defmacro nvp-with-async-override (orig-fn new-fn &rest body)
  "Set `symbol-function' of ORIG-FN to NEW-FN in process-buffer of 
BODY."
  (declare (indent defun))
  `(with-sentinel-wrapper
    (lambda (fn)
      (let ((fun fn))
        (lambda (p m)
          (cl-letf (((symbol-function ,orig-fn) ,new-fn))
            (funcall fun p m)))))
    ,@body))

(defmacro nvp-install--script (directory)
  "Find installation script."
  `(cl-loop for dir in '("script" "tools")
      for dirname = (expand-file-name dir ,directory)
      when (file-exists-p dirname)
      return (cl-loop for file in '("install.sh" "install")
                for fname = (expand-file-name file dirname)
                if (file-exists-p fname)
                return fname)))

(defmacro nvp-with-install-script (dir &optional funcs sudo &rest body)
  "Run installation script."
  (declare (indent defun) (debug defun))
  `(progn
     (require 'nvp)
     (require 'nvp-log)
     (require 'nvp-ext)
     (declare-function nvp-log "nvp-log")
     (let ((script (nvp-install--script ,dir)))
       (nvp-with-process-log
         (funcall-interactively 'nvp-ext-run-script script
                                ,(if (stringp funcs) `(cons ,funcs nil)
                                   funcs)
                                ,sudo)
         :pop-on-error
         ,@body))))

(defmacro nvp-with-script (script &optional funcs sudo &rest body)
  "Run FUNCS in SCRIPT."
  (declare (indent defun) (debug defun))
  `(progn
     (require 'nvp)
     (require 'nvp-log)
     (require 'nvp-ext)
     (declare-function nvp-log "nvp-log")
     (nvp-with-process-log
       (funcall-interactively 'nvp-ext-run-script ,script
                              ,(if (stringp funcs) `(cons ,funcs nil)
                                 funcs)
                              ,sudo)
       :pop-on-error
       ,@body)))

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
;;; Interactive Functions

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
                  collect `(and (looking-back ,open
                                              (line-beginning-position))
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
          (nvp-compile-basic)))
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

;;; REPLS

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
                  ;; add a sentinel to write comint history before other
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
;;; URL

(defmacro with-url-buffer (url &rest body)
  "Do BODY in buffer with contents from URL."
  (declare (indent defun)
           (debug (symbolp &rest form)))
  `(with-current-buffer (url-retrieve-synchronously ,url)
     ,@body))

(defmacro scan-url (url regex &rest body)
  "Do BODY in buffer with URL contents at position of REGEX."
  (declare (indent defun)
           (debug (symbolp symbolp &rest form)))
  `(with-url-buffer ,url
     (goto-char (point-min))
     (while (re-search-forward ,regex nil t)
       ,@body)
     (kill-buffer)))

;; -------------------------------------------------------------------
;;; Advice

;; from prelude
(defmacro advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS. The body of the advice
is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice
                      ,command
                      (,class
                       ,(intern (concat (symbol-name command) "-" advice-name))
                       activate)
                    ,@body))
               commands)))

;; -------------------------------------------------------------------
;;; Compat

(unless (fboundp 'ignore-errors)
  (defmacro ignore-errors (&rest body)
    `(condition-case nil (progn ,@body) (error nil))))

;; -------------------------------------------------------------------
;;; Variables / Declares

(defmacro nvp-local-vars ()
  `(progn
     (defvar nvp/abbrevs)
     (defvar nvp/auto)
     (defvar nvp/auto-site)
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
     (defvar nvp/config)
     (defvar nvp/lisp)
     (defvar nvp/mode)
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
     (defvar nvp/books)))

(provide 'nvp-macro)
;;; nvp-macro.el ends here
