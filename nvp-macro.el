;;; nvp-macro.el --- compile time macros -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-04-09.20>
;; Created:  2 November 2016

;;; Commentary:
;; info on declare - defun-declarations-alist, macro-declarations-alist
;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'macroexp)
(require 'nvp-macs-common "macs/nvp-macs-common")
(require 'nvp-macs-setup "macs/nvp-macs-setup")
(require 'nvp-macs-bindings "macs/nvp-macs-bindings")
(require 'nvp-macs-process "macs/nvp-macs-process")


;; -------------------------------------------------------------------
;;; Prefix args

(cl-defmacro nvp-prefix (num &optional then &rest else &key test &allow-other-keys)
  "If `current-prefix-arg' equals NUM do THEN otherwise ELSE."
  (declare (indent 2) (debug t))
  (while (keywordp (car else))
    (setq else (cdr (cdr else))))
  (let ((test-fn (if test (if (eq 'quote (car-safe test)) (cadr test)) 'eq)))
    (if (or then else)
        `(if (,test-fn ,num (prefix-numeric-value current-prefix-arg))
             ,then
           ,@else)
      `(,test-fn ,num (prefix-numeric-value current-prefix-arg)))))


;; -------------------------------------------------------------------
;;; Messages

(cl-defmacro nvp-msg (fmt &rest args &key keys delay duration &allow-other-keys)
  "Print message, optionally using `substitute-command-keys' if KEYS.
Message is displayed temporarily, restoring any previous message after
DURATION or 2 seconds.
If DELAY is non-nil, message won't be displayed for that many seconds, so
if a useful message is expected it can be read before this message is 
displayed. The original message will be displayed after DELAY + DURATION when
those are both specified."
  (declare (indent defun) (debug (sexp &rest form)))
  (while (keywordp (car args))
    (setq args (cdr (cdr args))))
  (let ((msg
         `(function
           (lambda ()
             (message
              ,@(if keys
                    `("%s" (substitute-command-keys (format ,fmt ,@args)))
                  `(,fmt ,@args)))))))
    `(let ((orig-msg (current-message)))
       ,(if delay
            `(run-with-idle-timer ,delay nil ,msg)
          `(funcall ,msg))
       (run-with-idle-timer
        (+ ,(or delay 0) ,(or duration 2))
        nil (function
             (lambda ()
               (eval
                `(and ,orig-msg (message ,orig-msg)))))))))

;; -------------------------------------------------------------------
;;; Buffer / Directory names

(defmacro nvp-buff--1 (path no-def)
  (if no-def
      `(file-name-directory (file-truename ,path))
    `(or (file-name-directory (file-truename ,path))
         (file-truename default-directory))))

(defmacro nvp-buff--2 (buff or-name)
  (if or-name
      `(or (buffer-file-name ,buff) (buffer-name ,buff))
    `(buffer-file-name ,buff)))

(cl-defmacro nvp-path (type &key buffer path no-default or-name)
  "Return file or directory name components specified by TYPE.
If BUFFER is non-nil, use its associated file (default current).
If PATH is non-nil, use it as the file path.
If NO-DEFAULT is non-nil, don't use `default-directory' if `buffer-file-name'
is nil.
If OR-NAME is non-nil, use `buffer-name' if `buffer-file-name' is nil.

`bn'     -- Buffer's name
`bfn'    -- Buffer file's full name
`bfns'   -- Buffer file's short name (no directory)
`bfne'   -- Buffer file's name w/o extension
`bfnd'   -- Buffer file's name or default directory
`bfnse'  -- Buffer file's short name w/o extension
`ext'    -- Buffer file extension
`dn'     -- Directory's short name (just the containing directory)
`dfn'    -- Full name of directory containing buffer file (no trailing slash)
`dfns'   -- Full directory name with trailing slash
`env'    -- Try to substitute environment variables in path, otherwise try to 
            expand path again when it is loaded.
"
  (let ((type (eval type)))
    (cond
     ;; buffer files
     ((eq type 'bn) `(buffer-name ,buffer))
     ((eq type 'bfn) `(nvp-buff--2 ,buffer ,or-name))
     ((eq type 'bfnd) `(or (nvp-buff--2 ,buffer ,or-name) default-directory))
     ((eq type 'bfns) `(file-name-nondirectory (nvp-buff--2 ,buffer ,or-name)))
     ((eq type 'bfne) `(file-name-sans-extension (nvp-buff--2 ,buffer ,or-name)))
     ((eq type 'bfnse) `(file-name-nondirectory
                         (file-name-sans-extension (nvp-buff--2 ,buffer ,or-name))))
     ((eq type 'ext) `(file-name-extension (nvp-buff--2 ,buffer ,or-name)))

     ;; directories
     ((eq type 'dn)
      (if (not path)
          `(file-name-nondirectory
            (directory-file-name
             (nvp-buff--1 buffer-file-name ,no-default)))
        `(if (directory-name-p ,path)
             (file-name-nondirectory
              (file-name-directory (file-truename ,path)))
           (directory-file-name
            (file-name-directory (file-truename ,path))))))

     ((eq type 'dfn)
      (if (not path)
          `(directory-file-name
            (nvp-buff--1 buffer-file-name ,no-default))
        `(if (directory-name-p ,path)
             (file-name-directory (file-truename ,path))
           (directory-file-name
            (file-name-directory (file-truename ,path))))))

     ((eq type 'dfns)
      (if (not path)
          `(nvp-buff--1 buffer-file-name ,no-default)
        `(if (directory-name-p ,path)
             (file-truename ,path)
           (file-name-directory (file-truename ,path)))))

     ((eq type 'env)
      (if (not path)
          (user-error "No path supplied with `env' to `nvp-path'.")
        (let ((res (substitute-env-in-file-name path)))
          (if (and res (file-exists-p res)) `,res
            `(substitute-env-in-file-name ,path))))))))


;; -------------------------------------------------------------------
;;; Syntax

(defmacro nvp-ppss (type &optional point ppss)
  "Return non-nil if syntax PPSS at POINT is of TYPE, one of the following.

`str'  -- Inside a string
`cmt'  -- Inside a comment
`soc'  -- Inside a string or comment"
  (let ((type (eval type)))
    (cond
     ((eq type 'str) `(nth 3 ,(or ppss `(syntax-ppss ,point))))
     ((eq type 'cmt) `(nth 4 ,(or ppss `(syntax-ppss ,point))))
     ((eq type 'soc)
      (macroexp-let2 nil syn (or ppss `(syntax-ppss ,point))
        `(or (car (setq ,syn (nthcdr 3 ,syn)))
             (car (setq ,syn (cdr ,syn)))
             (nth 3 ,syn)))))))

(defmacro nvp-if-ppss (type then &rest else)
  "Do THEN if syntax at point is of TYPE, otherwise ELSE."
  `(if (nvp-ppss ,type) ,then ,@else))

(defmacro nvp-unless-ppss (type &rest body)
  (declare (indent defun) (debug t))
  `(unless (nvp-ppss ,type)
     ,@body))

(defsubst nvp-between-empty-parens-p (&optional point)
  "Non-nil if POINT is between open/close syntax with only whitespace."
  (ignore-errors
    (and point (goto-char point))
    (and
     (progn (skip-syntax-forward " ") (eq ?\) (char-syntax (char-after))))
     (progn (skip-syntax-backward " ") (eq ?\( (char-syntax (char-before)))))))


;; -------------------------------------------------------------------
;;; Scan lists

;; cc-defs `c-safe-scan-lists', paredit
(defmacro nvp-safe-scan-lists (from count depth &optional limit)
  "`scan-lists', but return nil instead of errors."
  (let ((res `(ignore-errors (scan-lists ,from ,count ,depth))))
    (if limit
        `(save-excursion
           (when ,limit
             ,(if (numberp count)
                  (if (< count 0)
                      `(narrow-to-region ,limit (point-max))
                    `(narrow-to-region (point-min) ,limit))
                `(if (< ,count 0)
                     (narrow-to-region ,limit (point-max))
                   (narrow-to-region (point-min) ,limit))))
           ,res)
      res)))

(defmacro nvp-scan-1 (type &optional pos limit)
  "Return position across various balanced expressions.
Start at POS if non-nil. Returns point at new position, or nil on failure.

`fl'  -- after forward balanced parens
`bl'  -- after backward list
`ufl' -- up forward list
`ubl' -- up backward list
`dfl' -- down forward list
`dbl' -- down list backward
"
  (let ((type (eval type)))
    (cond
     ;; Return the point at different locations or nil
     ((eq type 'fl)
      `(when-let ((dest (nvp-safe-scan-lists ,(or pos '(point)) 1 0 ,limit)))
         (goto-char dest)
         dest))
     ((eq type 'bl)
      `(when-let ((dest (nvp-safe-scan-lists ,(or pos '(point)) -1 0 ,limit)))
         (goto-char dest)
         dest))
     ((eq type 'ufl)
      `(nvp-safe-scan-lists ,(or pos '(point)) 1 1 ,limit))
     ((eq type 'ubl)
      `(nvp-safe-scan-lists ,(or pos '(point)) -1 1 ,limit))
     ((eq type 'dfl)
      `(nvp-safe-scan-lists ,(or pos '(point)) 1 -1 ,limit))
     ((eq type 'dbl)
      `(nvp-safe-scan-lists ,(or pos '(point)) -1 -1 ,limit))

     (t (user-error "%S unrecognized by `nvp-scan-point'" type)))))

;; FIXME: use smie if available
(defmacro nvp-scan (type &optional pos limit)
  "Scan balanced expressions, moving and returning point on success.

`fl'   -- move forward balanced parens
`bl'   -- move backward list
`ufl'  -- point up list forward
`ubl'  -- point up list backward
`dfl'  -- point down list forward
`dbl'  -- point down list backward
`gufl' -- move up forward list
`gulb' -- move up backward list
`gdlf' -- move down list forward
`gdlb' -- move down list backward
"
  (let ((type (eval type)))
    (cond
     ((eq type 'fl) `(nvp-scan-1 'fl ,pos ,limit))
     ((eq type 'bl) `(nvp-scan-1 'bl ,pos ,limit))
     ((eq type 'ufl) `(nvp-scan-1 'ufl ,pos ,limit))
     ((eq type 'ubl) `(nvp-scan-1 'ubl ,pos ,limit))
     ((eq type 'dfl) `(nvp-scan-1 'dfl ,pos ,limit))
     ((eq type 'dbl) `(nvp-scan-1 'dbl ,pos ,limit))
     
     ;; Move point
     ((eq type 'gulf)
      `(when-let ((dest (nvp-scan-1 'ufl ,pos ,limit)))
         (goto-char dest) dest))
     ((eq type 'gulb)
      `(when-let ((dest (nvp-scan-1 'ubl ,pos ,limit)))
         (goto-char dest) dest))
     ((eq type 'gdlf)
      `(when-let ((dest (nvp-scan-1 'lf ,pos ,limit)))
         (goto-char dest) dest))
     ((eq type 'gdlb)
      `(when-let ((dest (nvp-scan-1 'lb ,pos ,limit)))
         (goto-char dest) dest))
     (t (user-error "%S unrecognized by `nvp-scan'" type)))))


;; -------------------------------------------------------------------
;;; Positions 
;; TODO: forward / backward comments only

;;-- Syntactic whitespace

;; move backward across newline (\r or \r\n), returning non-nil if moved
(defsubst nvp-backward-nl ()
  (cond
   ((eq (char-before) ?\r)
    (backward-char)
    t)
   ((and (eq (char-before) ?\n)
         (eq (char-before (1- (point))) ?\r))
    (backward-char 2)
    t)
   (t nil)))

;; see `c-forward-comments'
(defsubst nvp-forward-sws (&optional escape)
  "Move past following whitespace and comments.
Line continuations, recognized by ESCAPE, are treated as whitespace as well."
  (and (integerp escape) (setq escape (char-to-string escape)))

  (let ((escaped-nl (concat escape "[\n\r]")))
    (while (or (forward-comment 5)
               (when (and escape (looking-at escaped-nl))
                 (forward-char (length escape))
                 t)))))

;; see `c-backward-comments'
(defsubst nvp-backward-sws (&optional escape)
  "Move backward across whitespace, comments, and line continuations."
  (and escape (stringp escape) (setq escape (string-to-char escape)))

  (let ((start (point)))
    (while (and (not (bobp))
                (if (let (moved-comment)
                      (while (and
                              (not (setq moved-comment (forward-comment -1)))
                              (nvp-backward-nl))) ; skip \r or \r\n
                      moved-comment)
                    ;; line continuations
                    (when (and escape
                               (looking-at "[\n\r]")
                               (eq (char-before) escape)
                               (< (point) start))
                      (backward-char)
                      t))))))

(defmacro nvp-skip-sws (direction &optional escape)
  "Skip `forward' or `backward' across comments, whitespace, and line \
continuations."
  (and escape (stringp escape) (setq escape (string-to-char escape)))
  (let ((direction (eval direction)))
    (cond
      ((eq direction 'forward) `(nvp-forward-sws ,escape))
      ((eq direction 'backward) `(nvp-backward-sws ,escape))
      (t (error "Don't know how to skip %S" direction)))))

;;-- Whitespace only
(defmacro nvp-skip-ws-forward (escape &optional limit)
  "Skip horizontal/vertical whitespace and escaped newlines following point."
  (if limit
      `(let ((limit (or ,limit) (point-max)))
         (while (progn
		  ;; skip-syntax-* doesn't count \n as whitespace..
                  (skip-chars-forward " \t\n\r\f\v" limit)
                  (when (and (eq (char-after) ,escape)
                             (< (point) limit))
                    (forward-char)
                    (or (eolp)
                        (progn (backward-char) nil))))))
    `(while (progn
              (skip-chars-forward " \t\n\r\f\v")
              (when (and (eq (char-after) ,escape))
                (forward-char)
                (or (eolp)
                    (progn (backward-char) nil)))))))

(defmacro nvp-skip-ws-backward (escape &optional limit)
  "Skip over any whitespace, comments, and escaped nls preceding point."
  (if limit
      `(let ((limit (or ,limit (point-min))))
	 (while (progn
		  ;; skip-syntax-* doesn't count \n as whitespace..
		  (skip-chars-backward " \t\n\r\f\v" limit)
		  (and (eolp)
		       (eq (char-before) ?\\)
		       (> (point) limit)))
	   (backward-char)))
    `(while (progn
	      (skip-chars-backward " \t\n\r\f\v")
	      (and (eolp)
		   (eq (char-before) ,escape)))
       (backward-char))))

(defmacro nvp-skip-ws (direction &optional escape limit)
  "Skip horizontal/vertical whitespace and escaped nls in DIRECTION from point.
If ESCAPE is non-nil, treat as line continuation character (default '\\').
LIMIT restricts the search space when non-nil.
Direction is one of `forward', `backward'."
  (if escape
      (and (stringp escape) (setq escape (string-to-char escape)))
    (setq escape ?\\))

  (let ((direction (eval direction)))
    (if (eq direction 'forward)
        `(nvp-skip-ws-forward ,escape ,limit)
      `(nvp-skip-ws-backward ,escape ,limit))))

;;-- Points
;; cc-defs
;; #<marker at 34444 in cc-defs.el.gz>
(defmacro nvp-point (position &optional point escape)
  "Return relative POSITION to POINT (default current point).
ESCAPE can be a string to match escaped newlines (default '\\').
POSITION can be one of the following symbols:

`bol'   -- beginning of line
`eol'   -- end of line
`eoll'  -- end of logical line (i.e. without escaped NL)
`bod'   -- beginning of defun
`eod'   -- end of defun
`boi'   -- beginning of indentation
`ionl'  -- indentation of next line
`iopl'  -- indentation of previous line
`bonl'  -- beginning of next line
`eonl'  -- end of next line
`bopl'  -- beginning of previous line
`eopl'  -- end of previous line
`bohws' -- beginning of horizontal whitespace (doesn't cross lines)
`bows'  -- beginning of whitespace (crossing lines)
`bosws' -- beginning of syntactic whitespace (ws, comments, escaped nls)
`eohws' -- end of horizontal whitespace
`eows'  -- end of whitespace (crossing lines)
`eosws' -- end of syntactic whitespace (ws, comments, escaped nls)

If the referenced position doesn't exist, the closest accessible point
to it is returned.  This function does not modify the point or the mark."
  (or escape (setq escape "\\\\"))
  (cl-assert (eq (car-safe position) 'quote) nil "Call with quoted 'position'")

  (let ((position (eval position)))
    (cond

     ((eq position 'bol)
      (if (not point)
	  '(line-beginning-position)
	`(save-excursion
	   ,@(if point `((goto-char ,point)))
	   (beginning-of-line)
	   (point))))

     ((eq position 'eol)
      (if (not point)
	  '(line-end-position)
	`(save-excursion
	   ,@(if point `((goto-char ,point)))
	   (end-of-line)
	   (point))))

     ((eq position 'eoll)
      `(save-excursion
	 ,@(if point `((goto-char ,point)))
	 (while (progn
		  (end-of-line)
		  (prog1 (eq (logand 1 (skip-chars-backward ,escape)) 1)))
	   (beginning-of-line 2))
	 (end-of-line)
	 (point)))

     ((eq position 'boi)
      `(save-excursion
	 ,@(if point `((goto-char ,point)))
	 (back-to-indentation)
	 (point)))

     ((eq position 'bod)
      `(save-excursion
	 ,@(if point `((goto-char ,point)))
         (beginning-of-defun)
         (and defun-prompt-regexp
              (looking-at defun-prompt-regexp)
              (goto-char (match-end 0)))
	 (point)))

     ((eq position 'eod)
      `(save-excursion
	 ,@(if point `((goto-char ,point)))
         (end-of-defun)
	 (point)))

     ((eq position 'bopl)
      (if (not point)
	  '(line-beginning-position 0)
	`(save-excursion
	   ,@(if point `((goto-char ,point)))
	   (forward-line -1)
	   (point))))

     ((eq position 'bonl)
      (if (not point)
	  '(line-beginning-position 2)
	`(save-excursion
	   ,@(if point `((goto-char ,point)))
	   (forward-line 1)
	   (point))))

     ((eq position 'eopl)
      (if (not point)
	  '(line-end-position 0)
	`(save-excursion
	   ,@(if point `((goto-char ,point)))
	   (beginning-of-line)
	   (or (bobp) (backward-char))
	   (point))))

     ((eq position 'eonl)
      (if (not point)
	  '(line-end-position 2)
	`(save-excursion
	   ,@(if point `((goto-char ,point)))
	   (forward-line 1)
	   (end-of-line)
	   (point))))

     ((eq position 'iopl)
      `(save-excursion
	 ,@(if point `((goto-char ,point)))
	 (forward-line -1)
	 (back-to-indentation)
	 (point)))

     ((eq position 'ionl)
      `(save-excursion
	 ,@(if point `((goto-char ,point)))
	 (forward-line 1)
	 (back-to-indentation)
	 (point)))

     ;; whitespace
     ((eq position 'bohws)
      `(save-excursion
	 ,@(if point `((goto-char ,point)))
         (skip-syntax-backward " ")
	 (point)))

     ((eq position 'bows)
      `(save-excursion
         ,@(if point `((goto-char ,point)))
         (nvp-skip-ws 'backward ,escape)))

     ((eq position 'bosws)
      `(save-excursion
         ,@(if point `((goto-char ,point)))
         (nvp-skip-sws 'backward ,escape)))
     
     ((eq position 'eohws)
      `(save-excursion
         ,@(if point `((goto-char ,point)))
         (skip-syntax-forward " ")))

     ((eq position 'eows)
      `(save-excursion
	 ,@(if point `((goto-char ,point)))
         (nvp-skip-ws 'forward ,escape)
	 (point)))

     ((eq position 'eosws)
      `(save-excursion
         ,@(if point `((goto-char ,point)))
         (nvp-skip-ws 'forward ,escape)
         (point)))

     (t (error "Unknown buffer position requested: %s" position)))))

;; paredit splicing reindent doesn't account for prompts
(defmacro nvp-preserving-column (&rest body)
  "Preserve point in column after executing BODY.
`paredit-preserving-column' doesn't properly account for minibuffer prompts."
  (declare (indent defun) (debug body))
  (let ((orig-indent (make-symbol "indentation"))
        (orig-col (make-symbol "column")))
    `(let ((,orig-col (if (eq major-mode 'minibuffer-inactive-mode)
                          (- (current-column) (minibuffer-prompt-width))
                        (current-column)))
           (,orig-indent (current-indentation)))
       (unwind-protect
           (progn ,@body)
         (let ((ci (current-indentation)))
           (goto-char
            (+ (point-at-bol)
               (cond ((not (< ,orig-col ,orig-indent))
                      (+ ,orig-col (- ci ,orig-indent)))
                     ((<= ci ,orig-col) ci)
                     (t ,orig-col)))))))))


;; -------------------------------------------------------------------
;;; Strings

(defmacro nvp-s (type &optional beg end)
  "Wrapper for region/buffer strings.

`rs'   -- region string no props
`rsp'  -- region string w/ props
`rb'   -- region bounds
`bs'   -- buffer string (no widening, no props)
`bsp'  -- same with props
`bsw'  -- buffer string w/ widening, no props
`bswp' -- same but w/ props"
  (let ((type (eval type)))
    (cond
     ((eq type 'rs)
      `(buffer-substring-no-properties
        ,(or beg '(region-beginning)) ,(or end '(region-end))))
     ((eq type 'rsp)
      `(buffer-substring ,(or beg '(region-beginning)) ,(or end '(region-end))))
     ((eq type 'rb)
      `(car (region-bounds)))
     
     ((eq type 'bs)
      `(buffer-substring-no-properties
        ,(or beg '(point-min)) ,(or end '(point-max))))
     ((eq type 'bsp)
      `(buffer-substring ,(or beg '(point-min)) ,(or end '(point-max))))
     ((eq type 'bsw)
      `(save-restriction
         (widen)
         (buffer-substring-no-properties
          ,(or beg '(point-min)) ,(or end '(point-max)))))
     ((eq type 'bswp)
      `(save-excursion
         (widen)
         (buffer-substring ,(or beg '(point-min)) ,(or end '(point-max)))))
     (t (user-error "%S unknown to `nvp-s'" type)))))


;; -------------------------------------------------------------------
;;; Regions / things-at-point

(cl-defmacro nvp-tap-bounds (&optional thing &key pulse)
  "Return `bounds-of-thing-at-point' and pulse region unless NO-PULSE."
  (if (null pulse)
      `(bounds-of-thing-at-point ,(or thing ''symbol))
    `(progn
       (declare-function nvp-indicate-pulse-region-or-line "")
       (let ((bnds (bounds-of-thing-at-point ,(or thing ''symbol))))
         (when bnds
           (prog1 bnds
             (nvp-indicate-pulse-region-or-line (car bnds) (cdr bnds))))))))

(cl-defmacro nvp-tap (type &optional tap beg end &key pulse)
  "Wrapper for bounds/contents of region/thing-at-points.
Things at point default to 'symbols unless TAP is non-nil.
By regions of things at point are pulsed if PULSE is non-nil.
If BEG and END are non-nil, they are used as region bounds instead of those listed
below.

`tap'   -- Thing string, no props
`tapp'  -- Thing string w/ props
`btap'  -- Bounds of thing at point
`dwim'  -- If region is active, region string, otherwise thing-at-point (no props)
`dwimp' -- Same, but with props
`bdwim' -- Bounds of region or thing at point"
  (let ((type (eval type)))
    (cond
     ((eq type 'tap) `(thing-at-point ,(or tap ''symbol) 'no-props))
     ((eq type 'tapp) `(thing-at-point ,(or tap ''symbol)))
     ((eq type 'btap) `(nvp-tap-bounds ,tap :pulse ,pulse))

     ((eq type 'dwim)
      (if (and beg end) `(buffer-substring-no-properties ,beg ,end)
        `(if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (thing-at-point ,(or tap ''symbol) 'noprops))))
     ((eq type 'dwimp)
      (if (and beg end) `(buffer-substring ,beg ,end)
        `(if (use-region-p)
             (buffer-substring (region-beginning) (region-end))
           (thing-at-point ,(or tap ''symbol)))))

     ((eq type 'bdwim)
      (if (and beg end) `(cons ,beg ,end)
        `(if (use-region-p) (car (region-bounds))
           (nvp-tap-bounds ,(or tap ''symbol) :pulse ,pulse))))
     
     (t (user-error "Unknown `nvp-tap' type %S" type)))))

(cl-defmacro nvp-with-region (beg end &optional thing &rest body
                                  &key pulse widen type &allow-other-keys)
  "Bind BEG END to dwim region bounds.
Uses region bounds if active, otherwise bounds of THING.
In WIDEN is non-nil, save restriction and widen before finding bounds."
  (declare (indent defun) (debug body))
  (while (keywordp (car body))
    (setq body (cdr (cdr body))))
  `(,@(if widen '(save-restriction (widen)) '(progn))
    (if-let* ((bnds (nvp-tap ,(or type ''bdwim) ,(or thing ''paragraph)
                             :pulse ,pulse)))
        (cl-destructuring-bind (,beg . ,end) bnds
          ,@body)
      (user-error "nvp-with-region didn't find any bounds"))))


;; -------------------------------------------------------------------
;;; Save/Restore envs 

(defmacro nvp-save-buffer-state (varlist &rest body)
  "Bind variables, `let*', in VARLIST and execute BODY.
State is then restored. See `c-save-buffer-state' and `save-buffer-state'."
  (declare (indent 1) (debug t))
  `(with-silent-modifications
     (let* ,varlist
       ,@body)))

(defmacro nvp-with-preserved-vars (vars &rest body)
  "Let bind VARS then execute BODY, so VARS maintain their original values.
VARS should be either a symbol or list or symbols."
  (declare (indent 1) (debug (form body)))
  `(cl-progv ,(if (listp vars) `,vars `(list ',vars)) nil
     (unwind-protect
         ,@body)))

;; from yasnippet #<marker at 126339 in yasnippet.el>
(defmacro nvp-letenv (env &rest body)
  "Evaluate BODY with bindings from ENV.
ENV is a lisp expression evaluating to list of (VAR FORM), where
VAR is a symbol and FORM is evaluated."
  (declare (indent 1) (debug (form body)))
  (let ((envvar (make-symbol "envvar")))
    `(let ((,envvar ,env))
       (cl-progv
           (mapcar #'car ,envvar)
           (mapcar (lambda (k-v) (eval (cadr k-v))) ,envvar)
         ,@body))))

;; -------------------------------------------------------------------
;;; Folding

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
;;; Control flow

(cl-defmacro nvp-toggled-if (then &rest rest &key this-cmd &allow-other-keys)
  "Do THEN if `last-command' wasn't `this-command', otherwise do REST \
and set `this-command' to nil so opposite happens next time."
  (declare (indent 1))
  (while (keywordp (car rest))
    (setq rest (cdr (cdr rest))))
  `(if (not (eq this-command last-command))
       ,then
     (prog1 (progn ,@rest)
       (setq this-command ,this-cmd))))

;; -------------------------------------------------------------------
;;; Display Results

(cl-defmacro nvp-with-results-buffer (&optional buffer-or-name &rest body
                                                &key font-lock &allow-other-keys)
  "Do BODY in temp BUFFER-OR-NAME as with `with-temp-buffer-window'.
Make the temp buffer scrollable, in `view-mode' and kill when finished."
  (declare (indent defun) (debug (sexp &rest form)))
  (while (keywordp (car body))
    (setq body (cdr (cdr body))))
  `(let (other-window-scroll-buffer)
     (nvp-window-configuration-save)
     (with-temp-buffer-window
      ,(or buffer-or-name '(help-buffer))
      t
      nil
      (with-current-buffer standard-output
        (setq other-window-scroll-buffer (current-buffer))
        ,@body
        ,@(when font-lock '((font-lock-mode) (font-lock-ensure)))
        (hl-line-mode)
        (view-mode-enter nil #'nvp-window-configuration-restore)))))

;; evil-with-view-list: #<marker at 154076 in evil-common.el>
(cl-defmacro nvp-with-view-list (&key
                                 name           ;buffer name
                                 mode-name      ;mode-line name
                                 format         ;`tabulated-list-format'
                                 entries        ;`tabulated-list-entries'
                                 select-action) ;function applied to row
  "View buffer in `tabulated-list-mode'."
  (declare (indent defun) (debug t))
  (let ((action (make-symbol "action")))
    `(progn
       (declare-function nvp-view-list-mode "")
       (let ((,action ,select-action)
            (bufname (concat "*" ,name "*"))
            (inhibit-read-only t))
        (and (get-buffer bufname)
             (kill-buffer bufname))
        (let ((buf (get-buffer-create bufname)))
          (with-current-buffer buf
            (setq tabulated-list-format ,format
                  tabulated-list-entries ,entries
                  action ,action)
            (nvp-view-list-mode)        ;inits lists
            (setq mode-name ,mode-name))
          (pop-to-buffer buff))))))

;;; Time

(defmacro nvp-file-older-than-days (file days)
  "non-nil if FILE last modification was more than DAYS ago."
  (declare (indent defun) (debug t))
  `(< (time-to-seconds
       (time-subtract (current-time) (nth 5 (file-attributes ,file))))
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
;;; Toggled Tip
(declare-function pos-tip-show "pos-tip")

;; TODO:
;; - use help buffer with xref?
;; - better popup formatting
(cl-defmacro nvp-with-toggled-tip (popup
                                   &key
                                   (help-key "h") ;key-binding for help-fn
                                   (help-fn t)    ;more help (t is default)
                                   bindings       ;additional bindings
                                   (timeout 45)   ;pos-tip timeout
                                   keep           ;keep transient map
                                   use-gtk        ;use gtk tooltips
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
;;; Wrapper functions

;;-- Wrapper functions to call mode-local values
(defmacro nvp-wrapper-function (symbol &optional doc)
  "Creates a simple wrapper function to call local SYMBOL function with \
list of args.
Optionally use DOC for function."
  (let ((fn (intern (string-remove-suffix "-function" (symbol-name symbol))))
        ;; (arg (make-symbol "arg"))
        )
    `(defun ,fn ()
       ,(or doc (format "Function to run local `%s'." symbol))
       (interactive)
       (setq prefix-arg current-prefix-arg)
       (call-interactively ,symbol))))

(defmacro nvp-wrapper-fuctions (&rest fun-docs)
  "Create list of wrapper functions.
FUN-DOCS is an alist of pairs of symbols with optional docs."
  (macroexp-progn
   (cl-loop for (sym . doc) in fun-docs
      collect `(nvp-wrapper-function ,sym ,doc))))


;; -------------------------------------------------------------------
;;; Caches 

;; Simple memoization / result caching
(cl-defmacro nvp-define-cache (func arglist &optional docstring &rest body
                                           &key local predicate cache
                                           &allow-other-keys)
  "Create a simple cache for FUNC results named FUNC or CACHE if non-nil. 
Cache is either defvar (possibly local) so is updated when set to nil,
or PREDICATE is non-nil and returns nil."
  (declare (indent defun) (debug (sexp sexp sexp &form body)) (doc-string 3))
  (while (keywordp (car body))
    (setq body (cdr (cdr body))))
  (let* ((fn (if (stringp func) (intern func) func))
         (cache (or cache fn)))
    `(progn
       ,(if local `(defvar-local ,cache nil)
          `(defvar ,cache nil))
       (defun ,fn ,arglist
         ,docstring
         (or (,@(if predicate `(and ,predicate) '(progn)) ,cache)
             (setq ,cache (progn ,@body)))))))

(defmacro nvp-define-cache-runonce (func arglist &optional docstring &rest body)
  "Define cache function that will only compute cache once."
  (declare (indent defun) (debug defun) (doc-string 3))
  (let ((cache (make-symbol "cache-runonce"))
        (fn (if (stringp func) (intern func) func)))
    `(defun ,fn ,arglist
       ,docstring
       (or (get ',fn ',cache)
           (let ((val (progn ,@body)))
             (prog1 val
               (put ',fn ',cache val)))))))


;; -------------------------------------------------------------------
;;; Obsolete: function generators
;; remove these
;;-- marking

;; FIXME: most of these should either be generic or act on local variables
;; instead of being defined many times
;; Marks
(defmacro nvp--mark-defun (&optional first-time &rest rest)
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

;;-- Newline

;; FIXME: Obsolete
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

;;-- Wrap

;; Create function to wrap region, inserting BEGIN at beginning,
;; AFTER at the end.
(defmacro nvp-wrap-fn (name doc begin end &optional interact-p)
  (declare (indent defun))
  (let ((fn (intern (if (symbolp name)
                        (symbol-name name)
                      name))))
    `(defun ,fn ()
       ,doc
       ,@(when interact-p
           '(interactive "r"))
       (when (region-active-p)
        (save-excursion
          (goto-char (region-beginning))
          (insert ,begin))
        (goto-char (region-end))
        (insert ,end)))))

;;-- REPL/hippie setup
;; FIXME: how to make more generic?
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
       (declare-function nvp-he-history-setup "nvp-hippie-history")
       (defun ,fn ()
         (setq comint-input-ring-file-name (expand-file-name ,histfile nvp/cache))
         (setq comint-input-ring-size ,size)
         (comint-read-input-ring)
         (add-hook 'kill-buffer-hook 'comint-write-input-ring nil 'local)
         (nvp-he-history-setup
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
                 ,(or repl-process '(nvp-buffer-process)) :src-buffer)
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

;; FIXME: Asyncify
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
  (declare (indent defun))
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
;;; Locals - silence compiler

(defmacro nvp-local-vars ()
  '(progn
     (defvar nvp/abbrevs)
     (defvar nvp/auto)
     (defvar nvp/auto-site)
     (defvar nvp/devel)
     (defvar nvp/site)
     (defvar nvp/modes)
     (defvar nvp/emacs)
     (defvar nvp/build)
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

(nvp-local-vars)

;; Doesn't work
;; (put 'require 'byte-hunk-handler 'byte-compile-file-form-require)
(when (functionp 'backtrace-frames)
  (when (assoc '(t byte-compile-file-form-require
                   ((require 'nvp-macro))
                   nil)
               (backtrace-frames))
    (message "Warning: package 'nvp-macro required at runtime")))

(provide 'nvp-macro)
;;; nvp-macro.el ends here
