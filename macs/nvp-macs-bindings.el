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

  (defvar nvp--bindings-fast-move
    '(("n"     . nvp-move-next5)
      ("p"     . nvp-move-prev5)
      ("C-l"   . recenter-top-bottom)
      ("k"     . scroll-up-command)
      ("i"     . scroll-down-command)
      ("SPC"   . forward-page)
      ("M-SPC" . backward-page)))
  
  (defvar nvp--bindings-search
    '(("o" . occur)
      ;; XXX: don't override ???
      ("/" . isearch-forward)
      ("?" . isearch-backward)))

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
       ("S-SPC" . scroll-down))))

  (defvar nvp--bindings-kill
    '(("q"       . quit-window)
      ("Q"       . nvp-buffer-kill-mode-buffers)
      ("C-c C-k" . kill-this-buffer)))

  ;; same as dired-toggle
  (defvar nvp--bindings-wgrep
    '(("C-x C-q" . wgrep-change-to-wgrep-mode)
      ("w"       . wgrep-change-to-wgrep-mode)))

  (defvar nvp--bindings-comint
    '(("C-c C-n" . comint-next-prompt)
      ("M-N"     . comint-next-prompt)
      ("C-c C-p" . comint-previous-prompt)
      ("M-P"     . comint-previous-prompt)
      ("C-c C-k" . comint-clear-buffer)
      ("M-s-p"   . comint-previous-matching-input-from-input)
      ("M-s-n"   . comint-next-matching-input-from-input)
      ("C-c C-s" . comint-stop-subjob)
      ("C-c C-d" . comint-send-eof))))

;;; Helpers

(defun nvp:-with-bindings (type)
  (let ((var (intern (concat "nvp--bindings-" (symbol-name type)))))
    (unless (boundp var)
      (error "%s bindings unknown" var))
    (symbol-value var)))

(defun nvp:-compose-bindings (bindings)
  (unless (listp bindings) (setq bindings (list bindings)))
  (cl-loop for bs in bindings
           append (nvp:-with-bindings bs)))

(defsubst nvp:msg-from-bindings (bindings &optional prefix)
  "Create message of \"PREFIX: [key] cmd, ...\" from list of cons BINDINGS."
  (or prefix (setq prefix "Transient: "))
  (let ((msg (if (listp bindings)
                 (concat
                  prefix
                  (mapconcat (lambda (b)
                               (format "[%S] %S" (car b) (cdr b)))
                             bindings ", "))
               prefix)))
    msg))


;; -------------------------------------------------------------------
;;; Bind keys

(defmacro nvp:kbd (key)
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
(defmacro nvp:bind (map key cmd &rest kwargs)
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
            ((plist-get kwargs :keymap)
             cmd)
            (t `#',cmd)))
        (m (if (keymapp map) `',map map))
        (enable (or (plist-get kwargs :when)
                    (plist-get kwargs :enable)))
        (filter (plist-get kwargs :filter)))
    `(progn
       (declare-function ,cmd "")
       ,(cond
         ((or enable filter)
          `(define-key ,m (nvp:kbd ,key)
                       '(menu-item
                         ,(format "maybe-%s" (or (car (cdr-safe c)) c))
                         nil :filter
                         ,(if filter `,filter
                            `(lambda (&optional _)
                               (when ,(cond
                                       ((functionp enable) `(funcall ',enable))
                                       ((symbolp enable) `,enable)
                                       (t (macroexp-progn enable)))
                                 ,c))))))
         (t `(define-key ,m (nvp:kbd ,key) ,c))))))


(defmacro nvp:rebind-this-command (command &optional keymap)
  "Rebing `this-command' to COMMAND.
Search only within KEYMAP, as defined by `where-is-internal', which see.
Defaults to only search global map."
  `(when (called-interactively-p 'interactive)
     (let* ((all-keys (where-is-internal this-command (or ,keymap global-map)))
            (seps (seq-group-by
                   (lambda (key)
                     (and (vectorp key)
                          (eq (elt key 0) 'menu-bar)))
                   all-keys))
            (keys (cdr (assq nil seps)))
            non-modified-keys)
       (dolist (key keys)
         (if (member (event-modifiers (aref key 0)) '(nil (shift)))
             (push key non-modified-keys)))
       (if (> 1 (length keys))
           (user-error "multiple bindings found for %S, not rebinding" this-command)
         (let ((key (key-description (car keys))))
           ;; FIXME: if multiple keymaps are passed, rebind in the one where
           ;; binding was found
           (define-key (or ,keymap global-map) (kbd key) ,command)
           (setq unread-command-events
                 (--map (cons t it) (listify-key-sequence (kbd key)))))))))



;;; Local, Transient, Overriding maps

;;; TODO: get rid of these
;; Overrides a minor mode keybinding for the local buffer by creating
;; or altering keymaps stored in buffer-local variable
;; `minor-mode-overriding-map-alist'.
(cl-defmacro nvp:use-minor-mode-overriding-map (mode &rest bindings
                                                     &key predicate
                                                     after-load
                                                     &allow-other-keys)
  "Override minor MODE BINDINGS using `minor-mode-overriding-map-alist'.
If PREDICATE is non-nil, only override bindings if when it evaluates to non-nil."
  (declare (indent defun))
  (while (keywordp (car bindings))
    (setq bindings (cdr (cdr bindings))))
  (let ((modemap (nvp:-normalize-modemap mode)))
    `(,@(if after-load `(with-eval-after-load ,after-load) '(progn))
      (,@(if predicate `(when ,predicate) '(progn))
       (let ((map (make-sparse-keymap)))
         (set-keymap-parent map ,modemap)
         ,@(cl-loop for (k . b) in bindings
                    collect `(nvp:bind map ,k ,b))
         (push (cons ,mode map) minor-mode-overriding-map-alist))))))

(cl-defmacro nvp:set-local-keymap (&rest bindings
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
          collect `(nvp:bind lmap ,k ,b))
     ,(if buffer `(with-current-buffer ,buffer
                    ,(if use '(use-local-map lmap)
                       `(set (make-local-variable ',keymap) lmap)))
        (if use '(use-local-map lmap)
          `(set (make-local-variable ',keymap) lmap)))))

;;; Create Keymaps 

(defmacro nvp:create-keymaps (leader &rest maps)
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
                     (nvp:bind ,leader ,key ',map)))))


;; -------------------------------------------------------------------
;;; General bindings

(defsubst nvp:wrap--prefix (mapname)
  (replace-regexp-in-string
   (concat "-\\(?:minor-\\)?\\(?:mode-\\)?" (regexp-opt '("map" "keymap")) "$")
   "" mapname))

(defsubst nvp:wrap--make-name (def &optional prefix)
  (intern (concat (or prefix "nvp") "/" (symbol-name def))))

(defmacro nvp:wrap-command (def &optional prefix no-this-command no-interactive)
  "Creates wrapper for command DEF. Useful when advising a command for
repetition that may get called by other unwanted routines."
  (let ((name (nvp:wrap--make-name (nvp:unquote def) prefix)))
    `(defun ,name ()
       (interactive)
       ,@(unless no-this-command
           `((setq this-command ',def)))
       ,(if no-interactive
            `(funcall ',def)
          `(call-interactively ',def)))))

(defmacro nvp:def-keymap (name &rest defs)
  "Wrap `defvar-keymap' with additional keywords \\=':wrap and \\=':wrap-pref.
These specify commands to be wrapped to allow for commands to be part of
multiple repeat maps."
  (declare (indent defun) (debug t))
  (let (wrap wrap-pref kwargs)
    (while (keywordp (car defs))
      (cond ((eq ':wrap (car defs)) (setq wrap (cadr defs)))
            ((eq ':wrap-pref (car defs)) (setq wrap-pref (cadr defs)))
            (t (push (cadr defs) kwargs)
               (push (car defs) kwargs)))
      (setq defs (cddr defs)))
    (let (wrapped)
      (when wrap
        (let ((prefix (or wrap-pref (nvp:wrap--prefix (symbol-name name)))))
          (when (eq t wrap)
            (setq wrap
                  (cl-loop for (_k b) on defs by #'cddr
                           for name = (symbol-name (nvp:unquote b))
                           collect (intern (string-remove-prefix
                                            (concat prefix "/") name)))))
          (dolist (fn wrap)
            (push `(nvp:wrap-command ,fn ,prefix 'no-this-command) wrapped))))
      `(progn ,@wrapped
              (defvar-keymap ,name ,@(append kwargs defs))))))

;; option to check for override?
;; do (when-let ((curr (lookup-key (current-global-map) `(nvp:kbd ,k)))))
;; (cl-assert t 'show-args (format "%k is assigned %S globally" k curr))
(cl-defmacro nvp:bindings (keymap &optional feature &rest bindings
                                  &key
                                  local buff-local minor
                                  autoload create prefix
                                  repeat indicate
                                  wrap wrap-pref
                                  parent
                                  with
                                  prefix-key
                                  &allow-other-keys)
  "Set KEYMAP BINDINGS after FEATURE is loaded.
BINDINGS are conses of (key . command).

Optional:
  AUTOLOAD specifies prefix key which autoloads map.
  CREATE   if non-nil, map is initialized as sparse keymap.
  PREFIX   defines map as a prefix command with PREFIX as its name if it is a
           string.
  WITH     specifies sets of bindings, from `nvp--bindings-*', to add to map.
           These bindings are overwritten by any conflicts in BINDINGS.
  PREFIX-KEY append sequence to each binding in map
  PARENT   Set KEYMAP's parent

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
  WRAP-PREF  Optional prefix for wrapped functions.

Buggy:
  LOCAL makes map local var.        -- changes bindings for ALL mode buffers!!
  BUFF-LOCAL makes map buffer-local -- INCORRECT
"
  (declare (indent defun) (debug t))
  (if (listp keymap)
      (macroexp-progn
       (cl-loop for km in keymap
          if (listp km)
          collect `(nvp:bindings ,(car km) ',(cdr km) ,@bindings)
          else
          collect `(nvp:bindings ,km ,feature ,@bindings)))
    (while (keywordp (car bindings))
      (setq bindings (cdr (cdr bindings))))
    (when with
      (setq bindings (append (cl-loop for b in (nvp:-compose-bindings with)
                                      unless (assoc-string (car b) bindings)
                                      collect b)
                             bindings)))
    (and prefix-key (setq prefix-key (eval prefix-key))) ;can be symbol
    ;; (and (symbolp keymap) (setq keymap (symbol-name keymap)))
    (let ((modemap (nvp:-normalize-modemap keymap minor))
          (mapname (nvp:as-string keymap)))
      `(progn
         ,(when (symbolp modemap)
            `(eval-when-compile (defvar ,modemap)))
         ,(when prefix
            `(define-prefix-command ',modemap nil ,(and (stringp prefix) `,prefix)))
         ,(when create
            `(defvar ,modemap (make-sparse-keymap)))
         ,(when autoload
            `(nvp:bind global-map ,autoload
                       (nvp:lam ()
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
            ;; repeated commands that are also wrapped will be replaced by
            ;; wrapped versions, so this should come before handling 'repeat'
            (cond
             ((listp wrap) wrap)
             ((eq wrap 'all)
              (setq wrap (--map (cdr it) bindings)))
             ((eq wrap t)
              (if (memq repeat '(all t))
                  (setq wrap (--map (cdr it) bindings)
                        repeat (copy-sequence wrap))
                (setq wrap (copy-sequence repeat)))))
            (let ((wrap-pref (or wrap-pref (nvp:wrap--prefix mapname))))
              (dolist (b bindings)
                (and (memq (cdr b) wrap)
                     (setf (cdr b) (nvp:wrap--make-name (cdr b) wrap-pref))))
              (when (listp repeat)
                (dolist (fn wrap)
                  (--when-let (memq fn repeat)
                    (setf (car it) (nvp:wrap--make-name fn wrap-pref)))))
              `(progn
                 ,(macroexp-progn
                   (cl-loop for fn in wrap
                            collect
                            `(nvp:wrap-command ,fn ,wrap-pref))))))
         ,(when repeat
            ;; 
            (when (memq repeat '(all t))
              (setq repeat (--map (cdr it) bindings)))
            (setq repeat (cl-remove-duplicates repeat))
            (let ((repeat-fn (intern (concat "nvp/repeat-" mapname))))
              `(progn
                 (nvp:def ,repeat-fn (&rest _args)
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
              ;; the `prefix-key' may be a variable defined after package is
              ;; loaded
              ,@(cl-loop for (k . b) in bindings
                         collect
                         `(nvp:bind ,modemap
                                    ,(if prefix-key
                                         `(vconcat (nvp:kbd ,prefix-key)
                                                   (nvp:kbd ,k))
                                       k)
                                    ,(if (consp b) (car b) b)
                                    ,@(if (consp b) (cdr b))
                                    ;; ,(if (consp b) `,@(cdr b))
                                    ))))
         ,(when parent
            `(,@(if (or create prefix (equal feature :now)) '(progn)
                 `(with-eval-after-load ,(or feature `',(intern mapname))))
              (set-keymap-parent ,modemap ,parent)))))))

(provide 'nvp-macs-bindings)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-macs-bindings.el ends here
