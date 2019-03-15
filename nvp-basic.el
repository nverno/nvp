;;; nvp-basic.el --- basic requires -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-03-14 22:48:19>
;; Created: 16 November 2016

;;; Commentary:
;; Required in init
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (require 'subr-x))
(require 'nvp)
(declare-function company-quickhelp-manual-begin "company-quickhelp")
(declare-function do-smooth-scroll "smooth-scrolling")
(declare-function minibuffer-keyboard-quit "delsel")
(nvp-declare "paredit"
  paredit-close-round paredit-find-comment-on-line paredit-move-past-close)

;; -------------------------------------------------------------------
;;; Movement

(defun nvp-move-next5 (&rest _ignored)
  (interactive)
  (forward-line 5))

(defun nvp-move-prev5 (&rest _ignored)
  (interactive)
  (forward-line -5))

;; add smooth-scrolling
(nvp-advise-commands 'do-smooth-scroll :after (nvp-move-next5 nvp-move-prev5))

(defun nvp-move-forward-defun ()
  (interactive)
  (or (not (eq this-command 'nvp-move-forward-defun))
      (eq last-command 'nvp-move-forward-defun)
      (and transient-mark-mode mark-active)
      (push-mark))
  (beginning-of-defun -1))

(defalias 'nvp-move-backward-defun 'beginning-of-defun)

;;--- Headings
;; these may vary by mode
(nvp-define-cache nvp-local-header-regex ()
  "Get or create header regex based on comment syntax."
  :local t
  :cache nvp-local-header-regex
  (let* ((comment (string-trim comment-start))
         (cs (regexp-quote comment))
         (multi (> (string-width comment) 1)))
    (if (not multi)
        ;; ignore things like ';;;###autoload'
        (format "^\\s-*%s%s\\(?:—\\|---\\|\*\\| |\\|%s\\)\\s-"
                cs cs cs)
      (format "^\\s-*%s\\(?:—\\|---\\|%s\\)\\s-" cs
              (regexp-quote (substring comment 1 2))))))

(defun nvp-move-forward-heading (&optional back error)
  (interactive)
  (condition-case nil
      (progn
        (forward-line (if back -1 1))
        (if back (re-search-backward (nvp-local-header-regex))
          (re-search-forward (nvp-local-header-regex)))
        (forward-line 0))
    (error
     (forward-line (if back 1 -1))
     (user-error (format "No %s headings" (if back "previous" "more")))
     (and error (signal error t)))))

(defun nvp-move-previous-heading (&optional error)
  (interactive)
  (nvp-move-forward-heading 'back error))

;; -------------------------------------------------------------------
;;; Newline DWIM 

;; add additional newline when between syntactic open/closer
(defun nvp-newline-dwim--parens (&optional arg)
  (save-excursion
    (when (nvp-between-empty-parens-p)
      (newline-and-indent)))
  (newline arg 'interactive))

;; add a comment continuation string when in nestable doc comments
(defun nvp-newline-dwim--comment (&optional _comment-continue))

;; generics with defaults - lisp modes don't do anything special
;;;###autoload
(cl-defgeneric nvp-newline-dwim-prefix (&optional arg)
  "Generic function to handle newline dwim in special contexts."
  (newline arg 'interactive))

;;;###autoload
(cl-defgeneric nvp-newline-dwim-comment (&optional arg _comment-cont)
  "Generic function to handle newline dwim in comments."
  (newline arg 'interactive))

;;;###autoload
(cl-defgeneric nvp-newline-dwim-default (&optional arg)
  "Generic function to handle newline dwim syntactically."
  (let ((syntax (syntax-ppss)))
    (cond
     ((nvp-in-string syntax)
      (newline arg 'interactive))
     ((nvp-in-comment syntax 'in-string)
      (nvp-newline-dwim-comment arg))
     ;; default to adding newline between paren delimiters
     (t (nvp-newline-dwim--parens arg)))))

(cl-defmethod nvp-newline-dwim-default
  (&context (major-mode emacs-lisp-mode) &optional arg _pairs)
  "Nothing special for lisp newlines."
  (newline arg 'interactive))

(defun nvp-newline-dwim (&optional arg)
  "Newline dwim.
Dispatches to generic handlers with ARG."
  (interactive "*P")
  (funcall-interactively 'nvp-newline-dwim-default arg))

;; -------------------------------------------------------------------
;;; Paredit

(defun nvp-paredit-close-round (&optional arg)
  "Close paren skipping over possible comments and call `expand-abbrev'.
With ARG use default behaviour, except also call `expand-abbrev'."
  (interactive "P")
  (expand-abbrev)
  (if arg (paredit-close-round)
    (let ((beg (point)) ;keep comment on same line
          (cmt (paredit-find-comment-on-line)))
      (paredit-move-past-close ?\))
      (and cmt (save-excursion
                 (unless (eq (line-number-at-pos) (line-number-at-pos beg))
                   (goto-char beg))
                 (insert (car cmt)))))))

;; -------------------------------------------------------------------
;;; Company

; FIXME: toggle off just pops up a new postip
(defun nvp-company-quickhelp-toggle ()
  "Toggle pos-tip help on/off."
  (interactive)
  (let ((x-gtk-use-system-tooltips nil))
    (or (x-hide-tip)
        ;;   (add-hook 'pre-command-hook #'company-pre-command nil t)
        ;; (remove-hook 'pre-command-hook #'company-pre-command t)
        ;; (cl-letf (((symbol-function 'company--electric-restore-window-configuration)
        ;;            #'ignore)))
        (company-quickhelp-manual-begin))))

;;; FIXME: check if backend is already there -- this could be a macro
(defun nvp-company-local (backend)
  "Make a buffer-local company backend."
  (set (make-local-variable 'company-backends)
       (push backend company-backends)))

;; -------------------------------------------------------------------
;;; IDO

(defun nvp-ido-refresh-homedir ()
  "Refresh completion for homedir while ido is finding a file."
  (interactive)
  (ido-set-current-directory "~/")
  (setq ido-exit 'refresh)
  (exit-minibuffer))

(defun nvp-ido-yank ()
  "Forward to `yank'."
  (interactive)
  (if (file-exists-p (current-kill 0))
      (ido-fallback-command)
    (yank)))

(defun nvp-ido-backspace ()
  "Forward to `backward-delete-char'.
On error (read-only), quit without selecting."
  (interactive)
  (condition-case nil
      (backward-delete-char 1)
    (error
     (minibuffer-keyboard-quit))))

(defun nvp-ido-beginning-of-input ()
  (interactive)
  (goto-char (minibuffer-prompt-end)))

(defun nvp-ido-throw-dired ()
  (interactive)
  (throw 'dired t))

;; -------------------------------------------------------------------
;;; Wrapper functions

;;; FIXME: implement
(nvp-wrapper-fuctions
 (nvp-check-buffer-function . nil)
 (nvp-repl-jump-function    . nil)
 (nvp-test-function         . nil)
 (nvp-tag-function          . nil))

;; -------------------------------------------------------------------
;;; Marks

(defun nvp-mark-defun (arg)
  "Mark defun, skipping preceding comments."
  (interactive "p")
  (let ((skip-comments (not (region-active-p))))
    (setq prefix-arg (max 1 (/ (lsh arg -1) 4)))
    (funcall nvp-mark-defun-function prefix-arg)
    (and skip-comments (comment-forward (point-max)))))

;; -------------------------------------------------------------------
;;; Align
(nvp-declare "" nvp-indicate-pulse-region-or-line)

;; ensure spaces when aligning
(define-advice align-regexp (:around (old-fn &rest args) "no-tabs")
  (let ((indent-tabs-mode nil))
    (apply old-fn args)))

(define-advice align (:around (old-fn &rest args) "no-tabs")
  (let ((indent-tabs-mode nil))
    (apply old-fn args)))

(defun nvp-align (&optional arg beg end)
  "Align buffer region b/w BEG and END, or call `nvp-mark-defun' if nil.
With a single prefix, align entire active region or buffer.
With double prefix, highlight changes that would occur."
  (interactive
   (cons (prefix-numeric-value current-prefix-arg)
         (if (region-active-p)
             (list (region-beginning) (region-end)))))
  (if (eq 4 arg)                        ;align entire region / buffer
      (if (and beg end) (align beg end)
        (align (point-min) (point-max)))
    (save-mark-and-excursion
      (unless (and beg end)
        (nvp-mark-defun 1)
        (setq beg (region-beginning) end (region-end)))
        (nvp-indicate-pulse-region-or-line beg end)
      (if (eq 16 arg)                     ;test alignment rule
          (call-interactively 'align-highlight-rule)
        (align beg end)))))

;; -------------------------------------------------------------------
;;; Bindings

(defun nvp-buffer-local-set-key (key cmd)
  "Like `local-set-key', but don't modify KEY in other buffers of same mode."
  (let ((lmap (make-sparse-keymap)))
    (set-keymap-parent lmap (current-local-map))
    (define-key lmap key cmd)
    (use-local-map lmap)))

;; https://stackoverflow.com/a/14769115/2415684
(defun nvp-local-set-minor-mode-key (mode key def)
  "Override a minor mode keybinding for the local buffer only using \
`minor-mode-overriding-map-alist'."
  (let* ((oldmap (cdr (assoc mode minor-mode-map-alist)))
         (newmap (or (cdr (assoc mode minor-mode-overriding-map-alist))
                     (let ((map (make-sparse-keymap)))
                       (set-keymap-parent map oldmap)
                       (push `(,mode . ,map) minor-mode-overriding-map-alist)))))
    (define-key newmap key def)))

;; -------------------------------------------------------------------
;;; Hooks

;; Added to `find-file-not-found-functions'. Create new directories for new files.
(defun nvp-find-create-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p 
		(format "directory `%s' does not exist! create it?" 
			parent-directory)))
      (make-directory parent-directory t))))

(provide 'nvp-basic)
;;; nvp-basic.el ends here
