;;; nvp-toggle.el --- toggle/insert stuff  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'files-x)
(nvp:decls)


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
;;; Text

(defvar-keymap nvp-repeat-toggle-tabify-map
  :repeat t
  "t"     #'nvp-toggle-tabify
  "<tab>" #'nvp-toggle-tab-width)

;;;###autoload
(defun nvp-toggle-tabify (&optional beg end)
  "Tab/Untabify buffer regions - full visible buffer with prefix, otherwise
the current paragraph."
  (interactive
   (nvp:tap-or-region 'bdwim
     (if (> (prefix-numeric-value current-prefix-arg) 1) 'buffer 'paragraph)
     :pulse t))
  (nvp:toggled-if (untabify beg end)
    (tabify beg end)))

;; from https://github.com/skeeto/.emacs.d/blob/master/lisp/extras.el
;;;###autoload
(defun nvp-toggle-tab-width ()
  "Cycle `tab-width'."
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
  (interactive (nvp:repeat-args (nvp:tap-or-region 'bdwim 'list :pulse t)))
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
      (translate-region beg end tbl)))
  (nvp:repeat-this-command))

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
  "'"  #'nvp-toggle-quotes
  "\"" #'nvp-toggle-quotes)

;;; TODO(09/23/24): escape / unescape toggle quotes
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
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-toggle.el ends here
