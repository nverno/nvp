;;; nvp-octave.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'octave)

;; ------------------------------------------------------------
;;; Install

; (nvp-with-gnu
;   ;; checks current / available versions and builds/installs if
;   ;; necessary (non windows)
;   (defun nvp-octave-install (arg)
;     (interactive "P")
;     (nvp-with-install-script mm-tools--dir "install_octave_deps" 'sudo
;       (nvp-with-asdf-install arg mm-tools--dir "octave")))
  
;   (defun nvp-octave-install-octave ()
;     (let ((script (expand-file-name "tools/install.sh" mm-tools--dir)))
;       (nvp-with-process-log
;         (nvp-ext-run-script script '("install_octave") 'sudo)
;         :pop-on-error)))
;   (defun nvp-octave-octave-build ()
;     (async-shell-command ". ~/bin/install/get-octave.sh" (nvp-process-buffer))))

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
(defvar nvp-octave--buffer)
(defun nvp-octave-switch ()
  (interactive)
  (if (eq major-mode 'inferior-octave-mode)
      (and nvp-octave--buffer
           (switch-to-buffer-other-window
            nvp-octave--buffer))
    (setq nvp-octave--buffer (current-buffer))
    (unless (buffer-live-p inferior-octave-buffer)
      (inferior-octave))
    (octave-show-process-buffer)))

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
    :help-fn #'(lambda (fn) (interactive) (octave-help fn))))

(provide 'nvp-octave.el)
;;; nvp-octave.el ends here