;;; nvp-macs-common.el --- basic macros -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'macroexp)
(require 'dash)
(require 'nvp-subrs "macs/nvp-subrs")

(defvar nvp-debug-level nil)

;; (defun nvp-debug (&rest args)
;;   (and nvp-debug-level (apply #'message args)))

(defmacro nvp:debug (&rest _args))

;; -------------------------------------------------------------------
;;; OS 

(defmacro nvp:with-w32 (&rest body)
  (declare (indent 0) (debug t))
  (when (eq system-type 'windows-nt)
    `(progn ,@body)))

(defmacro nvp:with-gnu (&rest body)
  (declare (indent 0) (debug t))
  (when (not (eq system-type 'windows-nt))
    `(progn ,@body)))

(defmacro nvp:with-gnu/w32 (gnu w32)
  (declare (indent 2) (indent 1) (debug t))
  (if (eq system-type 'windows-nt)
      `,@w32
    `,@gnu))

;; -------------------------------------------------------------------
;;; Utils: Conversion / Normalization

(defun nvp:-normalize-modemap (mode &optional minor)
  "Convert MODE to keymap symbol if necessary.
If MINOR is non-nil, create minor mode map symbol."
  (and (eq 'quote (car-safe mode)) (setq mode (eval mode)))
  (cond
   ((and (stringp mode) (eq (intern mode) 'global-map))
    (list 'current-global-map))
   ((keymapp mode) mode)
   (t (setq mode (nvp:as-string mode))
      (let ((minor (or minor (string-match-p "-minor" mode))))
        (if (not (or (string-match-p "-map\\'" mode)
                     (string-match-p "-keymap\\'" mode)))
            (intern
             (concat (replace-regexp-in-string "\\(?:-minor\\)?-mode\\'" "" mode)
                     (if minor "-minor-mode-map" "-mode-map")))
          (intern mode))))))

(defun nvp:-normalize-hook (mode &optional minor)
  "Convert MODE to canonical hook symbol.
If MINOR is non-nil, convert to minor mode hook symbol."
  (and (eq 'quote (car-safe mode)) (setq mode (eval mode)))
  (setq mode (nvp:as-string mode))
  (let ((minor (or minor (string-match-p "-minor" mode))))
    (intern
     (concat
      (replace-regexp-in-string
       "\\(?:-minor\\)?\\(?:-mode\\)?\\(?:-hook\\)?\\'" "" mode)
      (if minor "-minor-mode-hook" "-mode-hook")))))


;; -------------------------------------------------------------------
;;; Normalize macro arguments

(defmacro nvp:skip-keywords (else)
  "Skip past any keywords in ELSE."
  `(while (keywordp (car ,else)) (setq ,else (cddr ,else))))

(defsubst nvp:arglist-remove-kwargs (args kwargs)
  "Return keyword arguments ARGS from KWARGS."
  (cl-loop for (k v) on kwargs by #'cddr
           unless (memq k args)
           nconc (list k v)))

(defsubst nvp:arglist-args (args)
  "Return arguments minus `cl--lambda-list-keywords' and any arguments prefixed
with \"_\"."
  (--filter (not (string-prefix-p "_" (symbol-name it)))
            (cl--arglist-args args)))

(defvar nvp-macs-merge-key-alist
  '((:if    . (lambda (new old) `(and ,new ,old)))
    (:after . (lambda (new old) `(:all ,new ,old))))
  "See `use-package-merge-key-alist'.")

;; default function to merge key values when there may be a default or multiple
;; found in arguments
(defun nvp:-macs-merge-keys (key new old)
  (let ((merger (assq key nvp-macs-merge-key-alist)))
    (if merger (funcall (cdr merger) new old)
      (append new old))))

(defun nvp:-normalize-plist (name input
                                      &optional plist defaults merge-function)
  "Normalize pseudo-plist to regular plist, extending key/value pairs.
Keywords will be call by a function nvp-macs-normalize/<keyword> with three
arguments: NAME, the keyword, and any args following before next keyword.
If the keyword still has no default and is a member of DEFAULTS, that will
be used. Modification of `use-package-normalize-plist'."
  (if (null input)
      plist
    (let* ((kw (car input))
           (xs (nvp:list-split-at #'keywordp (cdr input)))
           (args (car xs))
           (tail (cdr xs))
           (normalizer
            (intern-soft
             (format "nvp-macs-normalize/%s/%s" (symbol-name name) (symbol-name kw))))
           (arg (if (functionp normalizer)
                    (funcall normalizer name kw args)
                  (car args))))
      (if (memq kw defaults)
          (progn
            (unless arg (setq arg (plist-get arg defaults)))
            (setq plist (nvp:-normalize-plist
                         name tail plist defaults merge-function))
            (plist-put plist kw
                       (if (and merge-function (plist-member plist kw))
                           (funcall merge-function kw arg (plist-get plist kw))
                         arg)))
        ;; unknown keyword
        (nvp:-normalize-plist name tail plist defaults merge-function)))))


(defun nvp:-normalize-keywords (name args &optional defaults merge-function)
  (let ((name-sym (nvp:as-symbol name)))
    (setq args (delq 'elisp--witness--lisp args))
    (let ((body (list nil)))
      (while (and args (not (keywordp (car args))))
        (push (car args) body)
        (setq args (cdr args)))
      (nvp:plist-merge 
       (nvp:-normalize-plist
        name-sym args `(:body ,(nreverse body)) defaults merge-function)
       defaults))))



;; -------------------------------------------------------------------
;;; General 

(defmacro nvp:unless-bound (sym &rest body)
  "Execute BODY unless SYM is `fboundp' or `boundp'."
  (declare (indent 1) (debug t))
  `(unless (fboundp ,sym)
     ,@body))

(defmacro nvp:when-bound (sym &rest body)
  "Execute BODY when SYM is `fbound' or `boundp'."
  (declare (indent 1) (debug t))
  `(when (fboundp ,sym)
     ,@body))

(defmacro nvp:with-gensyms (syms &rest body)
  "Execute BODY with SYMS bound to `cl-gensyms'."
  (declare (indent 1))
  `(let ,(mapcar (lambda (s)
                   ;; see helm-lib's `helm-with-gensym' for explanation
                   ;; of using `cl-gensym' as opposed to `make-symbol'
                   `(,s (cl-gensym (symbol-name ',s))))
                 syms)
     ,@body))

(defmacro nvp:with-syms (syms &rest body)
  (declare (indent 1) (debug t))
  `(let ,(mapcar (lambda (s) `(,s (make-symbol (symbol-name ',s)))) syms)
     ,@body))

;; -------------------------------------------------------------------
;;; Functions

(defmacro nvp:alias (syms &optional original)
  "Create aliases for SYMS. If ORIGINAL, save original definitions.
If entry in SYMS is a cons cell, alias car to its cdr. Otherwise, entries
are aliases to symbols prefixed by \"nvp-\"."
  (setq syms (nvp:list-unquote syms))
  (macroexp-progn
   (cl-loop for sym in syms
            as symbol = (if (consp sym) (car sym) sym)
            as alias = (if (consp sym) (cdr sym)
                         (intern (concat "nvp-" (symbol-name symbol))))
            as orig = (intern (concat (symbol-name alias) "-orig"))
            nconc `(,@(if original
                          `((defalias ',orig (symbol-function #',symbol))))
                    (defalias ',symbol (symbol-function #',alias))))))

(defmacro nvp:lam (match-form &rest body)
  "Return interactive lambda, destructuring with `-lambda'."
  (declare (indent defun) (debug t))
  (let (ispec doc doc-ispec)
    (when (and (> (length body) 1)
               (stringp (car-safe body)))
      (setq doc (pop body)))
    (when (eq (car-safe (car-safe body)) 'interactive)
      (setq ispec (pop body)))
    (setq doc-ispec (delq nil `(,doc ,ispec)))
    (cond
     ((null match-form)
      `(lambda nil ,@doc-ispec ,@body))
     ((-all? 'symbolp match-form)
      `(lambda ,match-form ,@doc-ispec ,@body))
     (t
      (let* ((inputs
              (--map-indexed
               (list it (make-symbol (format "input%d" it-index))) match-form)))
        `(lambda ,(--map (cadr it) inputs)
           ,@doc-ispec
           (-let* ,inputs ,@body)))))))

(defmacro nvp:def (func match-form &rest body)
  "Define function and return it."
  (declare (indent defun)
           (debug (&define name lambda-list
                           [&optional stringp]
                           [&rest sexp]
                           def-body)))
  `(progn
     (defalias ',func (nvp:lam ,match-form ,@body))
     (declare-function ,func "")
     #',func))

(defmacro nvp:! (fun)
  "Return function negating result of FUN."
  `(lambda (&rest args) (not (apply ,fun args))))

(defmacro nvp:with-letf (old-fn new-fn &rest body)
  "Simple wrapper around `cl-letf' to execute BODY."
  (declare (indent 2) (debug t))
  (when (eq 'quote (car-safe new-fn))
    (setq new-fn `(symbol-function ',(cadr new-fn))))
  `(cl-letf (((symbol-function ,old-fn) ,new-fn)) ,@body))

(defmacro nvp:with-letfs (bindings &rest body)
  "Temporarily override function definitions."
  (declare (indent 1)
           (debug ((&rest [&or (symbolp form) (gate gv-place &optional form)])
                   body)))
  (let ((fn-bindings
         (cl-loop for (old new) in (nvp:list-unquote bindings)
                  nconc `(((symbol-function ',(nvp:unquote old))
                           ,(if (eq 'quote (car-safe new))
                                `(symbol-function ',(cadr new))
                              `,new))))))
    `(cl-letf ,fn-bindings ,@body)))

(defmacro nvp:compose (expr)
  "Combine functions in EXPR without explicit `funcall's."
  `#',(nvp:-rbuild expr))

;;; TODO: !!
(defun nvp:-compose (&rest fns)
  "Compose FNS, eg. 𝔣𝔬𝔤(x) = "
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (cl-reduce #'funcall fns :from-end t :initial-value (apply fn1 args))))
    #'identity))

(defun nvp:-rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda)) expr
    (if (eq (car expr) 'nvp:-compose)
        (nvp:-build-compose (cdr expr))
      (nvp:-build-call (car expr) (cdr expr)))))

(defun nvp:-build-call (op fns)
  (let ((g (cl-gensym)))
    `(lambda (,g)
       (,op ,@(mapcar #'(lambda (f) `(,(nvp:-rbuild f) ,g)) fns)))))

(defun nvp:-build-compose (fns)
  (let ((g (cl-gensym)))
    `(lambda (,g)
       ,(cl-labels ((rec (fns)
                         (if fns
                             `(,(nvp:-rbuild (car fns))
                               ,(rec (cdr fns)))
                           g)))
          (rec fns)))))

;; -------------------------------------------------------------------
;;; Anamorphs
;; See ch. 14 of On Lisp

(defmacro nvp:awhile (expr &rest body)
  "Anamorphic `while'."
  (declare (indent 1) (debug t))
  (nvp:with-gensyms (flag)
    `(let ((,flag t))
       (cl-block nil
         (while ,flag
           (--if-let ,expr
               (progn ,@body)
             (setq ,flag nil)))))))

(defmacro nvp:acond (&rest clauses)
  "Anamorphic `cond'."
  (declare (debug cond))
  (unless (null clauses)
    (let ((cl1 (car clauses))
          (sym (cl-gensym)))
      `(let ((,sym ,(car cl1)))
         (if ,sym
             (let ((it ,sym)) ,@(cdr cl1))
           (nvp:acond ,@(cdr clauses)))))))

(defmacro nvp:alambda (params &rest body)
  "Anamorphic `lambda', binding the function to 'self'"
  (declare (indent defun) (debug t))
  `(cl-labels ((self ,params ,@body))
     #'self))

;; -------------------------------------------------------------------
;;; Structs / CLOS

(defvar eieio--known-slot-names)
(defmacro eieio-declare-slot (name)
  (cl-pushnew name eieio--known-slot-names) nil)

;; bind cl-defstruct slots
;; (defsubst nvp-cache--names (struct-type)
;;   (declare (pure t) (side-effect-free t))
;;   (let* ((class (cl--struct-get-class struct-type))
;;          (slots (cl--struct-class-slots class)))
;;     (cl-loop for i across slots
;;        collect (cl--slot-descriptor-name i))))

;; (defun nvp-struct--oref (struct slot)
;;   (declare (compiler-macro
;;             (lambda (exp)
;;               (ignore struct slot)
;;               exp)))
;;   (aref struct (cl-struct-slot-offset (type-of struct) slot)))

;; (defsubst nvp-struct--tag (struct)
;;   (aref struct 0))

;; TODO: make this work like `with-slots', so type doesn't need to be
;;       specified at compile-time. Problem is how to make the BODY setf-able.
;; eieio with-slots: #<marker at 12490 in eieio.el.gz>
(defmacro nvp:with-struct-slots (spec-list inst &rest body)
  "See `with-slots' for CLOS explanation."
  (declare (indent 2) (debug t))
  (macroexp-let2 nil inst inst
    `(cl-symbol-macrolet
         ,(mapcar (lambda (entry)
                    (let* ((var (if (listp entry) (car entry) entry))
                           (slot (if (listp entry) (cadr entry) entry))
                           ;; (idx (cl-struct-slot-offset (type-of inst) slot))
                           )
                      (list var `(eieio-oref ,inst ',slot))))
                  spec-list)
       ,@body)))

;; (defclass foo2 ()
;;   ((a :initform "a")
;;    (b :initform "b")
;;    (c :initform nil)))
;; (setq tst2 (make-instance 'foo2))
;; (with-slots (a b c) tst2
;;   (setq c (concat a b)))
;; tst2

;; (cl-defstruct foo a b c)
;; (setq tst (make-foo :a "a" :b "B"))
;; (with-slots (a b c) tst
;;   (setq c (concat a b)))

;; (nvp:with-struct-slots (a b c) tst
;;   (setq c (concat a b)))

;; -------------------------------------------------------------------
;;; Declares / Autoloads
;; silence byte-compiler warnings

(defalias 'nvp:decl 'nvp:declare)
(put 'nvp:decl 'lisp-indent-function 'defun)

(defmacro nvp:declare (&rest funcs)
  (declare (indent defun) (debug t))
  (let ((pkg ""))
    (when (stringp (car funcs))
      (setq pkg (car funcs)
            funcs (nvp:plist-delete (cdr funcs) :pkg)))
    (-let (((&plist :body funcs :pkg pkg :pre pre)
            (nvp:-normalize-keywords "decl" funcs `(:pkg ,pkg :pre nil))))
      (setq funcs (nvp:list-unquote funcs))
      (when pre
        (setq funcs (mapcar (lambda (fn)
                              (let ((fn (nvp:as-string fn)))
                                (if (string-prefix-p pre fn) fn
                                  (intern (concat pre "-" fn)))))
                            funcs)))
      (macroexp-progn
       (cl-loop for func in funcs
          collect `(declare-function ,func ,pkg))))))

(defalias 'nvp:auto 'nvp:autoload)
(put 'nvp:auto 'lisp-indent-function 'defun)
(defmacro nvp:autoload (package &rest funcs)
  (declare (indent defun))
  (setq funcs (nvp:list-unquote funcs))
  (macroexp-progn
   (cl-loop for func in funcs
      collect `(autoload ',func ,package))))

(defmacro nvp:setq-local (&rest var-vals)
  (declare (indent 0))
  (macroexp-progn
   (cl-loop for (var val) on var-vals by #'cddr
      collect `(setq-local ,var ,val))))


;; -------------------------------------------------------------------
;;; Strings / Regex

(defmacro nvp:wrap-with (pre post &rest body)
  (macroexp-let2* nil ((pre pre) (post post))
    `(concat ,pre ,(macroexp-progn body) ,post)))

;; not that useful -- concat only happens one first load
(defmacro nvp:concat (&rest body)
  `(eval-when-compile (concat ,@body)))

(defmacro nvp:re-opt (opts &optional no-symbol)
  `(eval-when-compile
     ,(if no-symbol `(regexp-opt ,opts t)
        `(nvp:wrap-with "\\_<" "\\_>" (regexp-opt ,opts t)))))


;; -------------------------------------------------------------------
;;; Files / buffers

;; modified from smartparens.el
;; (defmacro nvp-with-buffers-using-mode (mode &rest body)
;;   "Execute BODY in every existing buffer using `major-mode' MODE."
;;   (declare (indent 1))
;;   `(dolist (buff (buffer-list))
;;      (when (provided-mode-derived-p ,mode (buffer-local-value 'major-mode buff))
;;        (with-current-buffer buff
;;          ,@body))))

;;; Load file name
(defmacro nvp:load-file-name ()
  "Expand to the file's name."
  '(cond
    (load-in-progress load-file-name)
    ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
     byte-compile-current-file)
    (t (buffer-file-name))))


;; -------------------------------------------------------------------
;;; Restrictions

(defmacro nvp:with-restriction (type &rest body)
  "Narrow to region TYPE and do BODY."
  (declare (indent 1))
  `(save-restriction
     ,@(pcase type
         (`'visible '((narrow-to-region (window-start) (window-end))))
         (_ (error "Invalid type: %S" type)))
     ,@body))


;; -------------------------------------------------------------------
;;; Input keys / IO

;; FIXME: best way to get last input char?
;; `key-description' is defined at C level and `edemacro-format-keys' does
;; a lot of work. How to profile?
;; - semantic-read-event : #<marker at 3072 in fw.el.gz>
(defmacro nvp:input (type)
  "Return user input by TYPE.
Trailing 's' indicates a string is returned. 
See Info node `(elisp) Input Events'.

* ~~~ Last command/input keys
`lcs'  -- last char from `last-command-event'
`lis'  -- last char from `last-input-event'
`lcks' -- keys (string) from `last-command-event' w/ ctrl chars stripped
`liks' -- keys (string) from `last-input-event'

* ~~~ Basic chars (no caps)
`lbi'  -- Last basic input char: `event-basic-type', `last-input-event'
`lbis' 
`lbc'  -- Last basic command char
`lbcs' 

* ~~~ Events
`lem'   -- `last-command-event' modifiers
`lec'   -- `last-command-event' click count 

* ~~~ Last command names
`lrcn' -- For now, just `last-repeatable-command' - probably should filter out
          uselss commands
`lcn'  -- Last command from last key in `this-command-keys-vector'."
  (let ((type (eval type)))
    (cond
     ;; === Last input key ===
     ((eq type 'lcs) '(substring (key-description (vector last-command-event)) -1))
     ((eq type 'lcks) '(key-description (vector last-command-event)))
     ((eq type 'lis) '(substring (key-description (vector last-input-event)) -1))
     ((eq type 'liks) '(key-description (vector last-input-event)))
     ;;              '(kbd (edmacro-format-keys (vector last-input-event)))

     ;; === Basic ====
     ((eq type 'lbi) `(event-basic-type last-input-event))
     ((eq type 'lbis) '(single-key-description (event-basic-type last-input-event)))
     ((eq type 'lbc) '(event-basic-type last-command-event))
     ((eq type 'lbcs) '(single-key-description (event-basic-type last-command-event)))
     
     ;; === Events ===
     ((eq type 'lem) (event-modifiers last-command-event))
     ((eq type 'lec) (event-click-count last-command-event))
     
     ;; === Last command (symbol) ===
     ((eq type 'lrcn) 'last-repeatable-command)
     ((eq type 'lcn)
      '(let* ((keys (this-command-keys-vector))
              (last-key (and (vectorp keys)
                             (aref keys (1- (length keys))))))
         (and last-key (lookup-key (current-active-maps 'olp) (vector last-key)))))

     (t (message "Unknown type `nvp:input': %S" type)))))

(provide 'nvp-macs-common)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-macs-common.el ends here
