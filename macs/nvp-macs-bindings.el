;;; nvp-macs-bindings.el --- bindings -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(require 'cl-lib)
(require 'subr-x)
(eval-when-compile (require 'nvp-macs-common))

;; -------------------------------------------------------------------
;;; Helpers

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
;;; Kbd wrapper

(defmacro nvp-kbd (key)
  "If key is a string, wrap with `kbd', otherwise leave it."
  (and (symbolp key) (setq key (symbol-value key)))
  ;; (and prefix (stringp prefix) (setq prefix (listify-key-sequence )))
  (cl-assert (or (vectorp key) (stringp key) (keymapp key)))
  (if (or (vectorp key) (keymapp key)) key (kbd key)))

(defmacro nvp-def-key (map key cmd)
  "Bind KEY, being either a string, vector, or keymap in MAP to CMD."
  `(progn
     (declare-function ,cmd "")
     (define-key
      ,(if (keymapp map) `',map map)
      (nvp-kbd ,key)
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
        (t `#',cmd)))))

;;-- Conditional binding
;; info: extended menu items
;; http://endlessparentheses.com/define-context-aware-keys-in-emacs.html
(defmacro nvp-defcond-key (map key def &rest body)
  "Creates a binding in MAP that is conditional on BODY."
  (declare (indent 3) (debug body))
  `(define-key ,map ,key
     '(menu-item
       ,(format "maybe-%s" (or (car (cdr-safe def)) def))
       nil
       :filter
       (lambda (&optional _)
         (when ,(macroexp-progn body)
           ,def)))))


;; -------------------------------------------------------------------
;;; Local, Transient, Overriding maps

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


(cl-defmacro nvp-use-transient-bindings
    (&optional bindings
               &key
               (keep t)
               (pre '(nvp-indicate-cursor-pre))
               (exit '(lambda () (nvp-indicate-cursor-post)))
               (repeat t) ;add repeat binding as last char
               repeat-key) ;key to use instead of last char
  "Set BINDINGS in transient map or REPEAT command.
If both BINDINGS and REPEAT are nil, do nothing.
Run PRE form prior to setting commands and EXIT on leaving transient map.
If REPEAT is non-nil, add a binding to repeat command from the last input char
or use REPEAT-KEY if specified."
  (declare (indent defun) (debug defun))
  (when (or bindings repeat)            ;w/o one of these, do nothing
    (let ((msg (nvp--msg-from-bindings bindings)))
     `(progn
        (nvp-declare "" nvp-indicate-cursor-pre nvp-indicate-cursor-post)
        ;; only set first time
        (when (null overriding-terminal-local-map)
          (let ((tmap (make-sparse-keymap))
                (repeat-key ,(when repeat (or repeat-key `(nvp-input 'lce)))))
            ,(when repeat
               (prog1 nil
                 (setq msg (concat msg (and bindings ", ") "[%s] repeat command"))))
            (message ,msg repeat-key)   ;echo bindings on first pass
            ,pre
            ,@(cl-loop for (k . b) in bindings
                 collect `(nvp-def-key tmap ,k ,b))
            ,(when repeat '(define-key tmap (kbd repeat-key) this-command))
            (set-transient-map
             tmap
             ,(if repeat `(lambda ()         ;conditions to continue
                            (when (and (not (memq this-command
                                                  '(handle-switch-frame
                                                    keyboard-quit)))
                                       (lookup-key tmap (this-single-command-keys)))
                              (message ,msg repeat-key) t))
                `,keep)
             ,exit)))))))

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
             collect `(nvp-def-key map ,k ,b))
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
          collect `(nvp-def-key lmap ,k ,b))
     ,(if buffer `(with-current-buffer ,buffer
                    ,(if use '(use-local-map lmap)
                       `(set (make-local-variable ',keymap) lmap)))
        (if use '(use-local-map lmap)
          `(set (make-local-variable ',keymap) lmap)))))


;; -------------------------------------------------------------------
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
                     (nvp-def-key ,leader ,key ',map)))))


;; -------------------------------------------------------------------
;;; General bindings
;; FIXME: merge all these to one generic macro

;;--- Global bindings
(cl-defmacro nvp-global-bindings (&rest bindings &key no-override &allow-other-keys)
  "Add BINDINGS to global keymap.
If NO-OVERRIDE is non-nil, assert that the new binding isn't already defined."
  (declare (indent defun) (debug t))
  (while (keywordp (car bindings))
    (setq bindings (cdr (cdr bindings))))
  (macroexp-progn
   (cl-loop for (k . b) in bindings
      when no-override
      do (when-let ((curr (lookup-key (current-global-map) `(nvp-kbd ,k)))))
        (cl-assert t 'show-args (format "%k is assigned %S globally" k curr))
      collect `(nvp-def-key (current-global-map) ,k ,b))))


(cl-defmacro nvp-bind-keys (map &rest bindings
                                &key predicate after-load
                                &allow-other-keys)
  "Add BINDINGS to MAP.
If PRED-FORM is non-nil, evaluate PRED-FROM before binding keys.
If AFTER-LOAD is non-nil, eval after loading feature/file."
  (declare (indent defun) (debug t))
  (while (keywordp (car bindings))
    (setq bindings (cdr (cdr bindings))))
  (let ((map (nvp--normalize-modemap map)))
    `(progn
       (eval-when-compile (defvar ,map))
       (,@(if predicate `(when ,predicate)
            (if after-load `(with-eval-after-load ,after-load) '(progn)))
        ,@(cl-loop for (k . b) in bindings
             collect `(nvp-def-key ,map ,k ,b))))))

(cl-defmacro nvp-bindings (mode &optional feature &rest bindings
                                &key local buff-local minor &allow-other-keys)
  "Set MODE BINDINGS after FEATURE is loaded.
If LOCAL is non-nil, make map buffer local."
  (declare (indent defun))
  (while (keywordp (car bindings))
    (setq bindings (cdr (cdr bindings))))
  (let ((modemap (nvp--normalize-modemap mode minor)))
    `(progn
       (eval-when-compile (defvar ,modemap))
       ,(when local           ; will change bindings for all mode buffers
          `(make-local-variable ',modemap))
       ,(when buff-local      ; HACK: but how to modify 
          `(with-no-warnings  ; something like `company-active-map'
             (make-variable-buffer-local ',modemap)))
       (with-eval-after-load ,(or feature `',(intern mode))
         ,@(cl-loop for (k . b) in bindings
              collect `(nvp-def-key ,modemap ,k ,b))))))


;; -------------------------------------------------------------------
;;; View bindings

;;; FIXME: define a keymap to inherit from, no need to define everytime
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
;;; Bindings to multiple modes

(cl-defmacro nvp-bindings-multiple-modes (modes &rest bindings
                                                &key view &allow-other-keys)
  "Add shared BINDINGS to multiple MODES keymaps.
MODES is of the form ((mode-name . feature) ...).
Optional :local key can be set to make the mappings buffer-local."
  (declare (indent 1))
  (while (keywordp (car bindings))
    (setq bindings (cdr (cdr bindings))))
  `(progn
     ,@(cl-loop for (mode . feature) in modes
          collect (if view
                      `(nvp-bindings-with-view ,mode ',feature ,@bindings)
                    `(nvp-bindings ,mode ',feature ,@bindings)))))


(provide 'nvp-macs-bindings)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-macs-bindings.el ends here
