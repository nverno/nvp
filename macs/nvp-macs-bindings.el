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
       ("SPC"   . scroll-down)
       ("S-SPC" . scroll-up)))))

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
;;; Bind keys

(defmacro nvp-kbd (key)
  "If key is a string, wrap with `kbd', otherwise leave it."
  (and (symbolp key) (setq key (symbol-value key)))
  ;; (cl-assert (or (vectorp key) (stringp key) (keymapp key)))
  (cond
   ((or (vectorp key) (keymapp key)) key)
   ((consp key) key)                    ;maybe '(kbd ...) already
   (t (kbd key))))

                                        ; Conditional binding
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
      collect `(nvp-bind (current-global-map) ,k ,b))))


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
             collect `(nvp-bind ,map ,k ,b))))))

(cl-defmacro nvp-bindings (mode &optional feature &rest bindings
                                &key local buff-local minor autoload create prefix
                                &allow-other-keys)
  "Set MODE BINDINGS after FEATURE is loaded.
If LOCAL is non-nil, make map buffer local.
AUTOLOAD specifies prefix key which autoloads map."
  (declare (indent defun))
  (while (keywordp (car bindings))
    (setq bindings (cdr (cdr bindings))))
  (and (symbolp mode) (setq mode (symbol-name mode)))
  (let ((modemap (nvp--normalize-modemap mode minor)))
    `(progn
       (eval-when-compile (defvar ,modemap))
       ,(when prefix
          `(define-prefix-command ',modemap nil ,(and (stringp prefix) `,prefix)))
       ,(when create
          `(defvar ,modemap (make-sparse-keymap)))
       ,(when autoload
          `(nvp-bind global-map ,autoload
                     (nvp-lam ()
                       (interactive)
                       (nvp-autoload-keymap
                        ',modemap ,(or feature `',(intern mode))))))
       ,(when local           ; will change bindings for all mode buffers
          `(make-local-variable ',modemap))
       ,(when buff-local      ; HACK: but how to modify 
          `(with-no-warnings  ; something like `company-active-map'
             (make-variable-buffer-local ',modemap)))
       ,(when bindings
          `(,@(if (or create prefix) '(progn)
                `(with-eval-after-load ,(or feature `',(intern mode))))
            ;; with-eval-after-load ,(or feature `',(intern mode))
            ,@(cl-loop for (k . b) in bindings
                 collect `(nvp-bind ,modemap ,k ,b)))))))

;;; View bindings

(defalias 'nvp-bindings-with-view 'nvp-bindings-modal-view)
(defmacro nvp-bindings-modal-view (mode &optional feature &rest bindings)
  (declare (indent defun))
  `(nvp-bindings ,mode ,feature
     ,@(eval-when-compile nvp--bindings-view)
     ,@bindings))

;;; Multiple Modes

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
