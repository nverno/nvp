;;; nvp-octave.el ---  -*- lexical-binding: t; -*-
;;; Commentary:

;;; TODO:
;; - quickhelp for completion candidates in source buffers
;; - complete for scalar structure fields - fieldnames(var)

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
