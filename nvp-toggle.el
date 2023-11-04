;;; nvp-toggle.el --- toggle/insert stuff  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'files-x)
(nvp:decls :p (time-stamp) :v (time-stamp-count))
(nvp:auto "nvp-util" 'nvp-regex-map-across-matches)

;;;###autoload
(defun nvp-toggle-timestamp (arg)
  "Insert/update timestamp for current buffer."
  (interactive "P")
  (require 'time-stamp)
  (let ((time-stamp-active t)
        (time-stamp-count (if arg (read-number "Max time stamps: ") 1))
        (time-stamp-pattern (or time-stamp-pattern
                                (pcase major-mode
                                  (`org-mode "#\\+DATE: <%%>$")
                                  (_ "15/Last modified: <%%>$")))))
    (time-stamp)))

;; -------------------------------------------------------------------
;;; Toggle file/directory local variables

;;;###autoload
(defun nvp-toggle-interpreter (interpreter)
  (interactive (list (read-string "Interpreter: ")))
  (let ((line (concat "#!/usr/bin/env " interpreter)))
    (save-excursion
      (goto-char (point-min))
      (if (looking-at-p line)
          (message "interpreter already %s" interpreter)
        (when (looking-at "^#!")
          (delete-region (point) (line-end-position)))
        (insert line)
        (save-buffer)
        (revert-buffer 'ignore-auto 'noconfirm)))))

;; remove empty prop-line -- point should be in -*- -*-
(defun nvp-toggle--cleanup-prop-line ()
  "Remove empty prop-line."
  (when (and (looking-at-p "[ \t]*-\\*-")
             (looking-back "-\\*-[ \t]*" (line-beginning-position)))
    (skip-chars-backward " \t\\*-" (line-beginning-position))
    (delete-region (point) (line-end-position))))

(eval-when-compile 
  ;; get value corresponding to variable which may be shortened, eg. `mode'
  (defsubst nvp:toggle--normalize-var (var)
    (pcase var
      (`mode major-mode)
      (`coding buffer-file-coding-system)
      (_ (if (and (symbolp var)
                  (boundp var))
             (symbol-value var)))))

  ;; normalize possible shortened mode names
  (defsubst nvp:toggle--normalize-mode (val)
    (let ((str (nvp:as-string val)))
      (if (string-match-p "-mode\\'" str) val
        (intern (concat str "-mode"))))))

;;;###autoload
(defun nvp-toggle-local-variable (var &optional val dir footer dir-mode)
  "Toggle file/dir-local binding of VAR with VAL.
If DIR is non-nil toggle dir-local variable, for DIR-MODE.
If FOOTER is non-nil toggle value in file's Local Variables."
  (setq var (nvp:as-symbol var))
  (let* ((curr (nvp:toggle--normalize-var var))
         (op (if (or (not curr)
                     (and val (not (equal curr (if (eq var 'mode)
                                                   (nvp:toggle--normalize-mode val)
                                                 val)))))
                 'add-or-replace
               'delete))
         (val (and (not (eq op 'delete)) (or val t)))
         (prop (or (member var '(mode lexical-binding))
                   (not (or dir footer))))
         (fn (cond
              (prop #'modify-file-local-variable-prop-line)
              (dir  #'modify-dir-local-variable)
              (t    #'modify-file-local-variable)))
         ;; (dir-mode (and (not prop) dir (read-file-local-variable-mode)))
         (start-state (buffer-chars-modified-tick))
         changed)
    (let ((orig-buff (current-buffer)))
      (save-excursion                   ;modify-file... moves point
        (apply fn (if dir (list dir-mode var val op) (list var val op)))
        (when (and prop (eq op 'delete))
          (nvp-toggle--cleanup-prop-line)))
      (setq changed (not (or (eq fn 'modify-dir-local-variable)
                             (eq start-state (buffer-chars-modified-tick)))))
      ;; save/revert buffer if changes were made
      (and changed (save-buffer))
      (if (not (or changed (eq fn 'modify-dir-local-variable)))
          (message "No changes local variables changed.")
        (with-current-buffer orig-buff ;dir-local modification changes buffer
          (revert-buffer 'ignore-auto 'no-confirm))
        (message "%s %s%s in the %s"
                 (if (eq op 'delete) "Deleted" "Updated")
                 (symbol-name var)
                 (if val (concat " => " (if (stringp val) val (symbol-name val))) "")
                 (if prop "prop-line" (if dir "dir-locals" "local vars")))))))

;;;###autoload
(defun nvp-toggle-file-local-binding (&optional footer)
  "Toggle file local binding.
If FOOTER is non-nil, use Local Variable list, otherwise -*- line."
  (interactive "P")
  (let ((var (read-file-local-variable "Toggle file-local variable")))
    (nvp-toggle-local-variable
     var (read-file-local-variable-value var) nil footer)))

;;;###autoload
(defun nvp-toggle-dir-local-binding (var val &optional mode)
  "Toggle dir-local binding of VAR to VAL for MODE."
  (interactive
   (let ((var (read-file-local-variable "Add dir-local variable")))
     (list var (read-file-local-variable-value var)
           (read-file-local-variable-mode))))
  (nvp-toggle-local-variable var val 'dir nil mode))

;; -------------------------------------------------------------------
;;; Font-lock / Chars

(defvar-keymap nvp-repeat-toggle-font-lock-map
  :repeat t
  "f" #'nvp-toggle-font-lock)

;;;###autoload
(defun nvp-toggle-font-lock (arg)
  "Toggle font-lock additions on/off in current buffer.
With prefix ARG, just refresh defaults."
  (interactive "P")
  (if arg
      (nvp:toggled-if (font-lock-refresh-defaults)
        (font-lock-flush)
        (font-lock-ensure))
    ;; otherwise, remove fonts added via `font-lock-add-keywords'
    (let ((mode-fonts (cdr (assq major-mode nvp-mode-font-additions))))
      (if (null mode-fonts)
          (user-error "No additional fonts for %S" major-mode)
        ;; if additional fonts are currently applied, there should be an
        ;; entry in `font-lock-keywords-alist'
        (if-let ((fonts (cdr (assq major-mode font-lock-keywords-alist))))
            (if (cl-some (lambda (elt)
                           (cl-find elt fonts :key #'caar :test #'equal))
                         mode-fonts)
                (font-lock-remove-keywords major-mode mode-fonts)
              ;; otherwise there are mode-fonts, but they have been removed or
              ;; were never added
              (font-lock-add-keywords major-mode mode-fonts))
          (font-lock-add-keywords major-mode mode-fonts))
        (font-lock-refresh-defaults)))))

;; -------------------------------------------------------------------
;;; Text

(defvar-keymap nvp-repeat-toggle-tabify-map
  :repeat t
  "t"     #'nvp-toggle-tabify
  "<tab>" #'nvp-toggle-tab-width)

(defun nvp-toggle-tabify (&optional beg end)
  "Tab/Untabify buffer regions - full visible buffer with prefix, otherwise
the current paragraph."
  (interactive
   (nvp:tap-or-region 'bdwim (nvp:prefix '>1 'buffer 'paragraph) :pulse t))
  (nvp:toggled-if (untabify beg end)
    (tabify beg end)))

;; from https://github.com/skeeto/.emacs.d/blob/master/lisp/extras.el
(defun nvp-toggle-tab-width ()
  (interactive)
  (let* ((widths [8 4 2])
         (m (or (cl-position tab-width widths) -1)))
    (setf tab-width (aref widths (mod (1+ m) (length widths)))))
  (message (format "tab-width: %d" tab-width)))

;; -------------------------------------------------------------------
;;; Delimiters: brackets / strings

;; translation table 
(nvp:define-cache-runonce nvp-toggle-brackets-table ()
  (let ((tbl (make-string 256 0)))
    (cl-loop for i from 0 upto 255
             do (aset tbl i i))
    tbl))

(defvar-keymap nvp-repeat-toggle-brackets-map
  :repeat t
  "b" #'nvp-toggle-brackets)

;;;###autoload
(defun nvp-toggle-brackets (&optional beg end)
  "Toggle b/w open/close braces, eg. ( => [ => {."
  (interactive (nvp:tap-or-region 'bdwim 'list :pulse t))
  (let ((bs '(?\[ ?\{ ?\())
        (tbl (nvp-toggle-brackets-table)))
    (-when-let (b (car (memq (char-after beg) bs)))
      (pcase b
        (?\( (aset tbl ?\( ?\[)
             (aset tbl ?\) ?\]))
        (?\[ (aset tbl ?\[ ?\{)
             (aset tbl ?\] ?\}))
        (?\{ (aset tbl ?\{ ?\()
             (aset tbl ?\} ?\))))
      (translate-region beg end tbl))))

;; toggle between quotes
(nvp:define-cache-runonce nvp-toggle-strings-table ()
  (let ((tbl (make-string 256 0)))
    (cl-loop for i from 0 upto 255
             do (aset tbl i i))
    tbl))

(defun nvp-bounds-of-string-at-point (&optional pt)
  (let ((ppss (nvp:ppss 'partial nil (or pt (point)))))
    (when (nth 3 ppss)
      (save-excursion
        (goto-char (nth 8 ppss))
        (ignore-errors (cons (point) (progn (forward-sexp) (point))))))))

(put 'string 'bounds-of-thing-at-point 'nvp-bounds-of-string-at-point)

(defvar-keymap nvp-repeat-toggle-quotes-map
  :repeat t
  "q" #'nvp-toggle-quotes)

;;; XXX: escape / unescape toggle quotes
;;;###autoload
(defun nvp-toggle-quotes (&optional beg end)
  "Toggle b/w quote styles."
  (interactive (nvp:tap-or-region 'bdwim 'string :pulse t))
  (when (and beg end)
    (let ((tbl (nvp-toggle-strings-table)))
      (pcase (char-after beg)
        (?\" (aset tbl ?\" ?\'))
        (?\' (aset tbl ?\' ?\")))
      (translate-region beg end tbl))))

(provide 'nvp-toggle)
;;; nvp-toggle.el ends here
