;;; nvp-octave.el ---  -*- lexical-binding: t; -*-
;;; Commentary:

;;; TODO:
;; - quickhelp for completion candidates in source buffers
;; - complete for scalar structure fields - fieldnames(var)
;; - '?' for help from repl

;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'octave)

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

;; -------------------------------------------------------------------
;;; REPL

;; switch between inferior / source buffer
(defun nvp-octave-switch ()
  (interactive)
  (let ((display-buffer-overriding-action
         '(display-buffer-pop-up-window ((inhibit-same-window  . t)))))
    (pop-to-buffer
     (if (eq major-mode 'inferior-octave-mode)
         (-if-let (proc (nvp-buffer-process))
             (let ((src-buf (process-get proc :src-buffer)))
               (or (and (buffer-live-p src-buf) src-buf)
                   (other-buffer (current-buffer) 'visible)))
           (other-buffer (current-buffer) 'visible))
       (let ((src-buf (current-buffer)))
         (unless (buffer-live-p inferior-octave-buffer)
           (inferior-octave))
         (process-put
          (get-buffer-process inferior-octave-buffer) :src-buffer src-buf)
         inferior-octave-buffer)))))

(defun nvp-octave-send-dwim (arg)
  (interactive "P")
  (if arg (octave-send-block) (octave-send-line)))

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
