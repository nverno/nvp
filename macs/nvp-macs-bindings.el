;;; nvp-macs-bindings.el --- bindings -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(require 'nvp-macs-common)

;;; Keys

(eval-and-compile
  (defvar nvp--bindings-hjkl
    '(("j" . next-line) ;; `next-line' often advised, unlike `forward-line'
      ("k" . previous-line)
      ("h" . backward-char)
      ("l" . forward-char)))

  (defvar nvp--bindings-move
    '(("M-n"   . nil) ;; use global defs.
      ("M-p"   . nil)
      ("M-s-n" . nil)
      ("M-s-p" . nil)
      ;; XXX: conditionally use `nvp-move-forward-defun' ??
      ;;      could check `beginning-of-defun-function', `defun-prompt-regexp'
      ;;      and if mode is a lisp derivative maybe
      ("M-N"   . nvp-move-forward-paragraph)
      ("M-P"   . nvp-move-backward-paragraph)))

  (defvar nvp--bindings-view
    (append
     nvp--bindings-hjkl
     nvp--bindings-move
     '(("e"     . end-of-line)
       ("a"     . beginning-of-line)
       ("A"     . beginning-of-buffer)
       ("E"     . end-of-buffer)
       ("/"     . isearch-forward)
       ("?"     . isearch-backward)
       ("SPC"   . scroll-up)
       ("i"     . scroll-down)
       ("S-SPC" . scroll-down)))))

;;; Helpers

(defun nvp--with-bindings (type)
  (let ((var (intern (concat "nvp--bindings-" (symbol-name type)))))
    (unless (boundp var)
      (error "%s bindings unknown" var))
    (symbol-value var)))

(defsubst nvp--msg-from-bindings (bindings &optional prefix)
  "Create message of 'PREFIX: [key] cmd, ...' from list of cons BINDINGS."
  (or prefix (setq prefix "Transient: "))
  (let ((msg (if (listp bindings)
                 (concat
                  prefix
                  (mapconcat (lambda (b)
                               (format "[%S] %S" (car b) (cdr b))) bindings ", "))
               prefix)))
    msg))


;; -------------------------------------------------------------------
;;; Bind keys

(defmacro nvp-kbd (key)
  "If key is a string, wrap with `kbd', otherwise leave it."
  ;; FIXME: doesn't do what intended I don't think -- check with undefined
  ;; prefix arg before package loads -- this should work in that case
  (if (symbolp key) `(kbd ,key)         ; evaluate at runtime instead
    ;; (and (symbolp key) (setq key (symbol-value key)))
    (cond
     ((null key) nil)
     ((or (vectorp key) (keymapp key)) key)
     ((consp key) (eval key))             ;maybe '(kbd ...) already or symbol
     (t (kbd key)))))

;;; Conditional binding
;; info: extended menu items
;; http://endlessparentheses.com/define-context-aware-keys-in-emacs.html
(defmacro nvp-bind (map key cmd &rest predicate)
  "Bind KEY, being either a string, vector, or keymap in MAP to CMD."
  (let ((c (cond
            ((or (null cmd) (and (consp cmd)
                                 (or (equal (cdr cmd) '(nil))
                                     (equal (cddr cmd) '(nil)))))
             nil)
            ((consp cmd)
             (cond
              ((equal (car cmd) 'function) cmd)
              ((equal (car cmd) 'quote) `#',(cadr cmd))
              (t cmd)))
            (t `#',cmd)))
        (m (if (keymapp map) `',map map)))
    `(progn
       (declare-function ,cmd "")
       ,(if predicate
            `(define-key ,m (nvp-kbd ,key)
               '(menu-item
                 ,(format "maybe-%s" (or (car (cdr-safe c)) c))
                 nil :filter (lambda (&optional _)
                               (when ,(if (symbolp predicate) predicate
                                        (macroexp-progn predicate))
                                 ,c))))
          `(define-key ,m (nvp-kbd ,key) ,c)))))


;;; Local, Transient, Overriding maps

;;; TODO: get rid of these
;; Overrides a minor mode keybinding for the local buffer by creating
;; or altering keymaps stored in buffer-local variable
;; `minor-mode-overriding-map-alist'.
(cl-defmacro nvp-use-minor-mode-overriding-map (mode &rest bindings
                                                     &key predicate
                                                     after-load
                                                     &allow-other-keys)
  "Override minor MODE BINDINGS using `minor-mode-overriding-map-alist'.
If PREDICATE is non-nil, only override bindings if when it evaluates to non-nil."
  (declare (indent defun))
  (while (keywordp (car bindings))
    (setq bindings (cdr (cdr bindings))))
  (let ((modemap (nvp--normalize-modemap mode)))
    `(,@(if after-load `(with-eval-after-load ,after-load) '(progn))
      ,@(if predicate `((when ,predicate)))
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map ,modemap)
        ,@(cl-loop for (k . b) in bindings
             collect `(nvp-bind map ,k ,b))
        (push (cons ,mode map) minor-mode-overriding-map-alist)))))

(cl-defmacro nvp-set-local-keymap (&rest bindings
                                         &key keymap buffer use &allow-other-keys)
  "Use or create a local version of KEYMAP (default `current-local-map').
If BUFFER is non-nil, use/set BINDINGS locally in BUFFER."
  (declare (indent defun))
  (while (keywordp (car bindings))
    (setq bindings (cdr (cdr bindings))))
  (cl-assert (if (not use) (not (null keymap)) t))
  `(let ((lmap (make-sparse-keymap)))
     (set-keymap-parent lmap ,(or keymap '(current-local-map)))
     ,@(cl-loop for (k . b) in bindings
          collect `(nvp-bind lmap ,k ,b))
     ,(if buffer `(with-current-buffer ,buffer
                    ,(if use '(use-local-map lmap)
                       `(set (make-local-variable ',keymap) lmap)))
        (if use '(use-local-map lmap)
          `(set (make-local-variable ',keymap) lmap)))))

;;; Create Keymaps 

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
                     (nvp-bind ,leader ,key ',map)))))


;; -------------------------------------------------------------------
;;; General bindings

(defsubst nvp-wrap--make-name (def)
  (intern (concat "nvp/" (symbol-name def))))

(defmacro nvp-wrap-command (def)
  "Creates wrapper for command DEF. Useful when advising a command for
repetition that may get called by other unwanted routines."
  (let ((name (nvp-wrap--make-name def)))
    `(defun ,name ()
       (interactive)
       (setq this-command ',def)
       (call-interactively ',def))))

;; option to check for override?
;; do (when-let ((curr (lookup-key (current-global-map) `(nvp-kbd ,k)))))
;; (cl-assert t 'show-args (format "%k is assigned %S globally" k curr))
(cl-defmacro nvp-bindings (keymap &optional feature &rest bindings
                                  &key
                                  local buff-local minor
                                  autoload create prefix
                                  repeat indicate wrap
                                  with
                                  prefix-key
                                  &allow-other-keys)
  "Set KEYMAP BINDINGS after FEATURE is loaded.
BINDINGS are conses of (key . command).

Optional:
  AUTOLOAD specifies prefix key which autoloads map.
  CREATE   if non-nil, map is initialized as sparse keymap.
  PREFIX   defines map as a prefix command with PREFIX as its name if it is a string.
  WITH     specifies sets of bindings, from 'nvp--bindings-*', to add to map. These
           bindings are overwritten by any conflicts in BINDINGS.
  PREFIX-KEY append sequence to each binding in map

Transient:
  REPEAT   A list of functions which will be advised to enter transient KEYMAP
            after they are called. 
           If REPEAT is t or \\='all, all functions in bindings will repeat.
  INDICATE Make cursor change color when transient bindings are active.
           Only effective with REPEAT.
  WRAP     A list of functions to create wrappers around. 
           If t, create wrappers for all REPEAT commands,
           or, if \\='all, create wrappers for all commands.
           Note: only wrapped commands will be advised.

Buggy:
  LOCAL makes map local var.        -- changes bindings for ALL mode buffers!!
  BUFF-LOCAL makes map buffer-local -- INCORRECT
"
  (declare (indent defun))
  (if (listp keymap)
      (macroexp-progn
       (cl-loop for km in keymap
          if (listp km)
          collect `(nvp-bindings ,(car km) ',(cdr km) ,@bindings)
          else
          collect `(nvp-bindings ,km ,feature ,@bindings)))
    (while (keywordp (car bindings))
      (setq bindings (cdr (cdr bindings))))
    (and with (setq bindings (append (nvp--with-bindings with) bindings)))
    (and prefix-key (setq prefix-key (eval prefix-key))) ;can be symbol
    ;; (and (symbolp keymap) (setq keymap (symbol-name keymap)))
    (let ((modemap (nvp--normalize-modemap keymap minor))
          (mapname (nvp-as-string keymap)))
      `(progn
         ,(when (symbolp modemap)
            `(eval-when-compile (defvar ,modemap)))
         ,(when prefix
            `(define-prefix-command ',modemap nil ,(and (stringp prefix) `,prefix)))
         ,(when create
            `(defvar ,modemap (make-sparse-keymap)))
         ,(when autoload
            `(nvp-bind global-map ,autoload
                       (nvp-lam ()
                         (interactive)
                         (nvp-autoload-keymap
                          ',modemap ,(or feature `',(intern mapname))))))
         ,(when local           ; will change bindings for all mode buffers
            `(make-local-variable ',modemap))
         ,(when buff-local      ; HACK: but how to modify 
            `(with-no-warnings  ; something like `company-active-map'
               (make-variable-buffer-local ',modemap)))

         ;; Transient bindings
         ,(when wrap
            (cond
             ((eq wrap 'all)
              (setq wrap (--map (cdr it) bindings)))
             ((eq wrap t)
              (if (memq repeat '(all t))
                  (setq wrap (--map (cdr it) bindings)
                        repeat (copy-sequence wrap))
                (setq wrap (copy-sequence repeat)))))
            (dolist (b bindings)
              (and (memq (cdr b) wrap)
                   (setf (cdr b) (nvp-wrap--make-name (cdr b)))))
            (dolist (fn wrap)
              (--when-let (memq fn repeat)
                (setf (car it) (nvp-wrap--make-name fn))))
            `(progn
               ,(macroexp-progn
                 (cl-loop for fn in wrap
                    collect `(nvp-wrap-command ,fn)))))
         
         ,(when repeat
            ;; 
            (when (memq repeat '(all t))
              (setq repeat (--map (cdr it) bindings)))
            (let ((repeat-fn (intern (concat "nvp/repeat-" mapname))))
              `(progn
                 (nvp-def ,repeat-fn (&rest _args)
                   ,(and indicate `(nvp-indicate-cursor-pre))
                   (set-transient-map
                    ,modemap t
                    ,(when indicate
                       `(lambda () (nvp-indicate-cursor-post)))))
                 ,(macroexp-progn
                   (cl-loop for fn in repeat
                      collect `(advice-add ',fn :after #',repeat-fn))))))

         ,(when bindings
            `(,@(if (or create prefix (equal feature :now)) '(progn)
                  `(with-eval-after-load ,(or feature `',(intern mapname))))
              ;; with-eval-after-load ,(or feature `',(intern mode))
              ;; the `prefix-key' may be a variable defined after package is loaded
              ,@(cl-loop for (k . b) in bindings
                   collect `(nvp-bind ,modemap
                                      ,(if prefix-key
                                           `(vconcat (nvp-kbd ,prefix-key)
                                                     (nvp-kbd ,k))
                                         k)
                                      ,b))))))))

(provide 'nvp-macs-bindings)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-macs-bindings.el ends here
