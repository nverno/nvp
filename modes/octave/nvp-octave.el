;;; nvp-octave.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; TODO:
;; - compilation error regex for repl error
;;   need to use 'which' to association filename with function
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'octave)
(nvp:decls :f (nvp-hap--docstring-from-buffer)
           :v (hs-special-modes-alist))

(with-eval-after-load 'hideshow
  (unless (assoc 'octave-mode hs-special-modes-alist)
    (push (list 'octave-mode
                (nvp:re-opt '("function" "for" "while" "if"))
                (nvp:re-opt '("end" "endfunction" "endfor" "endwhile" "endif"))
                "[%#]")
          hs-special-modes-alist)))

;; -------------------------------------------------------------------
;;; REPL

(defun nvp-octave-send-dwim (arg)
  (interactive "P")
  (if arg (octave-send-block) (octave-send-line)))

(with-eval-after-load 'nvp-repl
  (nvp-repl-add '(octave-mode)
    :modes '(inferior-octave-mode)
    :bufname inferior-octave-buffer
    :init (lambda ()
            (save-window-excursion
              (inferior-octave)
              (get-buffer-process inferior-octave-buffer)))))

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
        (condition-case nil
            (octave-help help-str)
          (error (message "no help found for '%s'" help-str)))
        (process-send-string proc "\n"))
    (comint-simple-send proc str)))

;; -------------------------------------------------------------------
;;; Completion

;; Fixes completion-at-point to work for fieldnames of structs/objects
(defun nvp-octave-bounds-of-object-at-point (&optional lim)
  (let ((beg (save-excursion
               (skip-syntax-backward "w_" lim)
               (while (and (not (<= (point) (or lim (line-beginning-position))))
                           (eq ?. (char-before)))
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

(defun nvp-octave--doc-buffer (str)
  (save-window-excursion
    ;; no need to call `octave-help' if help for STR is already being displayed
    ;; in buffer
    (unless (and (get-buffer octave-help-buffer)
                 (with-current-buffer octave-help-buffer
                   (string= str (cadr help-xref-stack-item))))
      (octave-help str))
    octave-help-buffer))

(defun nvp-octave-completion-at-point ()
  (-let (((beg . end)
          (nvp-octave-bounds-of-object-at-point (line-beginning-position))))
    (when beg
      (list beg end
            (or (and (inferior-octave-process-live-p)
                     (inferior-octave-completion-table))
                octave-reserved-words)
            :company-doc-buffer #'nvp-octave--doc-buffer))))

(defun nvp-inferior-octave-completion-at-point ()
  (unless (string-match-p "/" (or (comint--match-partial-filename) ""))
    (-let (((beg . end)
            (nvp-octave-bounds-of-object-at-point
             (comint-line-beginning-position))))
      (when beg
        (list beg end (completion-table-in-turn
                       (inferior-octave-completion-table)
                       'comint-completion-file-name-table)
              :company-doc-buffer #'nvp-octave--doc-buffer)))))

;; override octave's completion
(defalias 'octave-completion-at-point #'nvp-octave-completion-at-point)
(defalias 'inferior-octave-completion-at-point
  #'nvp-inferior-octave-completion-at-point)

;; -------------------------------------------------------------------
;;; Help

(defun nvp-octave--docstring (fn)
  (with-current-buffer (nvp-octave--doc-buffer fn)
    (nvp-hap--docstring-from-buffer (point-min))))

;; display for thing at point in popup tooltip
(defun nvp-octave-help-at-point (fn)
  (interactive (list (thing-at-point 'symbol)))
  (nvp:with-toggled-tip (nvp-octave--docstring fn)
    :help-fn (lambda ()
               (interactive)
               (display-buffer (nvp-octave--doc-buffer fn) t))))

;; -------------------------------------------------------------------
;;; Commands

;;; Run

(defun nvp-octave-run ()
  (interactive)
  (shell-command
   (concat (nvp:with-gnu/w32 "./" "octave ")
           (file-name-nondirectory (buffer-file-name)))))

;;; Compile

(defun nvp-octave-compile ()
  (interactive)
  (setq-local compilation-read-command nil)
  (let ((compile-command
         (concat (nvp:with-gnu/w32 "./" "octave ")
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

(provide 'nvp-octave)
;;; nvp-octave.el ends here
