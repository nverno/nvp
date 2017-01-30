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

;; -------------------------------------------------------------------
;;; Misc

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

(defmacro nvp-bfn ()
  "Short buffer file name"
  `(file-name-nondirectory (file-name-sans-extension (buffer-file-name))))

(defmacro nvp-dfn ()
  "Short directory file name."
  `(file-name-nondirectory
    (directory-file-name
     (file-name-directory
      (file-name-sans-extension (buffer-file-name))))))

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
;;; Config

(defmacro nvp-program (name &optional no-compile)
  (let ((name (cond
               ((symbolp name) (symbol-name name))
               ((consp name)
                (pcase name
                  (`(quote ,sym)
                   (symbol-name sym))
                  (_ name)))
               ((stringp name) name)
               (t (user-error "%S unmatched")))))
    `(,(if no-compile 'progn 'eval-when-compile)
      (if (eq system-type 'windows-nt)
          (bound-and-true-p
           ,(intern (concat "nvp-" name "-program")))
        (let ((local (expand-file-name ,name "/usr/local/bin")))
          (if (file-exists-p local) local
            (executable-find ,name)))))))

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

(defmacro nvp-bindings (mode &optional feature &rest bindings)
  (declare (indent defun))
  (let ((modemap (intern (concat mode "-map"))))
    `(eval-after-load ,(or feature `',(intern mode))
       '(progn
          ,@(cl-loop for (k . b) in bindings
               collect `(define-key ,modemap (kbd ,k) ',b))))))

;; do BODY with BINDINGS set in transient map
(cl-defmacro nvp-with-temp-bindings ((&key (keep t)
                                           exit bindings)
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
  ''(("j"    . forward-line)
     ("k"    . previous-line)
     ("h"    . backward-char)
     ("l"    . forward-char)
     ("e"    . end-of-line)
     ("a"    . beginning-of-line)
     ("A"    . beginning-of-buffer)
     ("E"    . end-of-buffer)
     ("M-n"  . nil)
     ("M-p"  . nil)
     ("M-N"  . nvp-basic-down-paragraph)
     ("M-P"  . nvp-basic-up-paragraph)))

(defmacro nvp-bindings-with-view (mode &optional feature &rest bindings)
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
  "Toggle POPUP in pos-tip."
  (declare (indent defun))
  `(progn
     (declare-function pos-tip-show "pos-tip")
     (declare-function nvp-basic-temp-binding "nvp-basic")
     (let ((x-gtk-use-system-tooltips ,use-gtk))
      (or (x-hide-tip)
          (let ((str ,popup))
            (pos-tip-show str nil nil nil 20)
            (nvp-basic-temp-binding
             "h"
             ,(or help-fn
                  '(function
                    (lambda ()
                      (interactive)
                      (x-hide-tip)
                      (with-current-buffer (get-buffer-create
                                            "*nvp-help*")
                        (insert str)
                        (view-mode-enter)
                        (pop-to-buffer (current-buffer)))))))
            (nvp-basic-temp-binding
             "q" #'(lambda () (interactive) (x-hide-tip)))
            ,@(cl-loop for (k . b) in bindings
                 collect `(nvp-basic-temp-binding (kbd ,k) ,b)))))))

;; -------------------------------------------------------------------
;;; Processes

(defmacro nvp-process-buffer (&optional comint &rest body)
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
  (declare (indent defun))
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

(defmacro nvp-with-install-script (dir &optional funcs sudo &rest body)
  "Run installation script."
  (declare (indent defun) (debug defun))
  `(progn
     (require 'nvp)
     (require 'nvp-log)
     (require 'nvp-ext)
     (let ((script (expand-file-name "tools/install.sh" ,dir)))
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
     (nvp-with-process-log
       (funcall-interactively 'nvp-ext-run-script ,script
                              ,(if (stringp funcs) `(cons ,funcs nil)
                                 funcs)
                              ,sudo)
       :pop-on-error
       ,@body)))

(defmacro nvp-with-asdf-install (prefix dir plugin &optional config-defaults
                                        error-callback success-callback
                                        script-fn sudo &rest body)
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
              (str (buffer-substring-no-properties start end))
              res)
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
