;;; nvp-octave.el ---  -*- lexical-binding: t; -*-
;;; Commentary:

;;; TODO:
;; - quickhelp for completion candidates in source buffers

;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'octave)
(nvp-decls)

;; -------------------------------------------------------------------
;;; REPL

(defun nvp-octave-send-dwim (arg)
  (interactive "P")
  (if arg (octave-send-block) (octave-send-line)))

(nvp-repl-add '(octave-mode)
  :modes '(inferior-octave-mode)
  :bufname inferior-octave-buffer
  :init (lambda ()
          (save-window-excursion
            (inferior-octave)
            (get-buffer-process inferior-octave-buffer))))

;; special processing for help commands, which are one of:
;; > help ("%s")
;; > help "%s"
;; > ? %s
(defun nvp-octave-input-sender (proc str)
  (-if-let
      (help-str
       (and (string-match "^ *help *(?\"\\([^\"]*\\)\")?\\|^ *\\? *\\(.+\\)" str)
            (or (match-string 1 str)
                (match-string 2 str))))
      (progn
        (octave-help help-str)
        (process-send-string proc "\n"))
    (comint-simple-send proc str)))

;; Fixes completion-at-point to work for fieldnames of structs/objects
(defun nvp-octave-bounds-of-object-at-point (&optional lim)
  (let ((beg (save-excursion
               (skip-syntax-backward "w_" lim)
               (when (eq ?. (char-before))
                 (forward-char -1)
                 (and (eq ?\) (char-before)) ; struct(i).
                      (ignore-errors (backward-sexp)))
                 (skip-syntax-backward "w_" lim))
               (point)))
        (end (point)))
    (when (< beg end)                   ; extends region past point
      (save-excursion
        (skip-syntax-forward "w_")
        (setq end (point))))
    (when (> end beg)
      (cons beg end))))

(defun nvp-octave-completion-at-point ()
  (-let (((beg . end)
          (nvp-octave-bounds-of-object-at-point (line-beginning-position))))
    (when beg
      (list beg end
            (or (and (inferior-octave-process-live-p)
                     (inferior-octave-completion-table))
                octave-reserved-words)))))

(defun nvp-inferior-octave-completion-at-point ()
  (unless (string-match-p "/" (or (comint--match-partial-filename) ""))
    (-let (((beg . end)
            (nvp-octave-bounds-of-object-at-point
             (comint-line-beginning-position))))
      (when beg
        (list beg end (completion-table-in-turn
                       (inferior-octave-completion-table)
                       'comint-completion-file-name-table))))))

;; override octave's completion
(defalias 'octave-completion-at-point #'nvp-octave-completion-at-point)
(defalias 'inferior-octave-completion-at-point
  #'nvp-inferior-octave-completion-at-point)

;; -------------------------------------------------------------------
;;; Commands

;;; Run

(defun nvp-octave-run ()
  (interactive)
  (shell-command
   (concat (nvp-with-gnu/w32 "./" "octave ")
           (file-name-nondirectory (buffer-file-name)))))

;;; Compile

(defun nvp-octave-compile ()
  (interactive)
  (setq-local compilation-read-command nil)
  (let ((compile-command
         (concat (nvp-with-gnu/w32 "./" "octave ")
                 (file-name-nondirectory buffer-file-name))))
    (call-interactively 'compile)))

;; ------------------------------------------------------------
;;; Insert / Toggle

(put 'nvp-octave-matrix 'no-self-insert t)

;; expand matrix with M rows from 'mm' abbrev
(defun nvp-octave-matrix ()
  (interactive)
  (let ((m (read-number "Rows: ")))
    (yas-expand-snippet
     (concat "[" (mapconcat #'(lambda (i) (format "[ $%d ]" i))
                            (number-sequence 1 m) ";\n") "]"))))

;; -------------------------------------------------------------------
;;; Help

;; display for thing at point in popup tooltip
(defun nvp-octave-help-at-point (fn)
  (interactive (list (thing-at-point 'symbol)))
  (nvp-with-toggled-tip
    (progn (inferior-octave-send-list-and-digest
            (list (format "help ('%s');\n" fn)))
           (mapconcat 'identity inferior-octave-output-list "\n"))
    :help-fn #'(lambda () (interactive) (octave-help fn))))

(provide 'nvp-octave)
;;; nvp-octave.el ends here
