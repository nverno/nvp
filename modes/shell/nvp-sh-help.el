;;; nvp-sh-help.el --- help-at-point for sh-script -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/shell-tools
;; Last modified: <2019-03-15 14:36:47>
;; Created:  5 December 2016

;;; Commentary:

;; - Within '[' or '[[' => show help for switches
;; - On shell builtin   => bash -c 'help %s'
;; - Otherwise          => use man

;; TODO:
;; - ${} formatting
;; - special variables

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib)
  (defvar Man--sections))
(require 'nvp-help) ;; parse 'man' stuff
(autoload 'Man-build-section-list "man")
(autoload 'pos-tip-show "pos-tip")

;; ignore ':', not symbolized to match strings
(defvar nvp-sh-help-bash-builtins
  (eval-when-compile
    (concat
     (nvp-re-opt
      '("." "[" "[[" "alias" "bg" "bind" "break" "builtin" "case" "cd"
        "command" "compgen" "complete" "continue" "declare" "dirs"
        "disown" "echo" "enable" "eval" "exec" "exit" "export" "fc" "fg"
        "getopts" "hash" "help" "history" "if" "jobs" "kill" "let"
        "local" "logout" "popd" "printf" "pushd" "pwd" "read" "readonly"
        "return" "set" "shift" "shopt" "source" "suspend" "test" "times"
        "trap" "type" "typeset" "ulimit" "umask" "unalias" "unset"
        "until" "wait" "while")
      'no-symbol)
     "\\'")))
  
;; -------------------------------------------------------------------
;;; Utilities

;; name of current command
(defun nvp-sh-help-current-command ()
  (let ((ppss (syntax-ppss))
        (start (or (cdr (bounds-of-thing-at-point 'symbol)) (point))))
    ;; ignore comments
    (when (not (nth 4 ppss))
      (save-excursion
        (catch 'done
          (while t
            (skip-chars-backward "^:<>)(|&\`;\[" (line-beginning-position))
            (if (or (not (nth 3 (syntax-ppss)))
                    (eq (char-before) ?\`)
                    (and (eq (char-before) ?\()
                         (eq (char-before (1- (point))) ?$)))
                (throw 'done nil)
              ;; move backward out of enclosing string that shouldn't be a quoted
              ;; command
              (up-list -1 t t))))
        (skip-syntax-forward " " start)
        (cond
         ;; '[[' or '['
         ((looking-back "\\(?:^\\|[^[]\\)\\(\\[+\\)[ \t]*"
                        (line-beginning-position))
          (match-string 1))
         ;; 'if' => if in situation like 'if ! hash', then
         ;; return 'hash'
         ((looking-at-p "if\\_>")
          (if (looking-at "if[ \t]+!?[ \t]*\\([-+[:alnum:]]+\\)")
              (match-string 1)
            "if"))
         ;; otherwise, return first symbol
         (t (and (looking-at "[:+_\[\.[:alnum:]-]+")
                 (match-string 0))))))))

;; get conditional switch
(defun nvp-sh-help-conditional-switch ()
  (save-excursion
    (skip-chars-backward "^\[" (line-beginning-position))
    (and (not (bolp))
         (looking-at "[ !]*\\(-[[:alpha:]]+\\)")
         (match-string 1))))

(defsubst nvp-sh-help-bash-builtin-p (cmd)
  (string-match-p nvp-sh-help-bash-builtins (regexp-quote cmd)))

;; synopsis: bash -c 'help -s %s'
;; help:     bash -c 'help %s'
(defsubst nvp-sh-help-bash-builtin-sync (cmd &optional synopsis)
  (shell-command-to-string
   (concat "bash -c 'help " (and synopsis "-s ") cmd "'")))

(defun nvp-sh-help-bash-builtin (cmd &optional sync buffer)
  (let ((cmd (concat "bash -c 'help " cmd "'"))
        (buffer (or buffer "*sh-help*")))
    (if sync
        (call-process-shell-command cmd nil buffer)
      (start-process-shell-command "bash" buffer cmd))))

;; output to BUFFER, return process
;; man --names-only %s | col -b
(defun nvp-sh-help-man (cmd &optional sync buffer)
  (let ((cmd (concat "man --names-only " (regexp-quote cmd) " | col -b")))
    (if sync
        (call-process-shell-command
         cmd nil (or buffer "*sh-help*"))
      (start-process-shell-command
       "man" (or buffer "*sh-help*") cmd))))

;; do BODY in buffer with man output
(defmacro sh-with-man-help (cmd &optional sync buffer &rest body)
  (declare (indent defun))
  `(let ((buffer (get-buffer-create (or ,buffer "*sh-help*"))))
     ,(if sync
          `(with-current-buffer buffer
             (nvp-sh-help-man ,cmd 'sync (current-buffer))
             ,@body)
        `(set-process-sentinel
          (nvp-sh-help-man ,cmd nil buffer)
          #'(lambda (p _m)
              (when (zerop (process-exit-status p))
                (with-current-buffer buffer
                  ,@body)))))))

;; if bash builtin do BASH else MAN
(defmacro sh-with-bash/man (cmd bash &rest man)
  (declare (indent 2) (indent 1))
  `(if (nvp-sh-help-bash-builtin-p ,cmd)
       ,bash
     ,@man))

;; process BODY in output of help for CMD. Help output is
;; either from 'bash help' for bash builtins or 'man'.
(defmacro sh-with-help (cmd &optional sync buffer &rest body)
  (declare (indent defun) (debug t))
  `(let ((buffer (get-buffer-create (or ,buffer "*sh-help*"))))
     (if ,sync
         (with-current-buffer buffer
             (apply (sh-with-bash/man ,cmd
                      'nvp-sh-help-bash-builtin
                      'nvp-sh-help-man)
                    ,cmd 'sync `(,buffer))
             ,@body)
       (set-process-sentinel
        (apply (sh-with-bash/man ,cmd 'nvp-sh-help-bash-builtin 'nvp-sh-help-man)
               ,cmd nil `(,buffer))
        #'(lambda (p _m)
            (when (zerop (process-exit-status p))
              (with-current-buffer buffer
                ,@body)))))))

;; -------------------------------------------------------------------
;;; Parse output

(defsubst nvp-sh-help--builtin-string ()
  (goto-char (point-min))
  ;; skip synopsis
  ;; (forward-line 1)
  (buffer-substring (point) (point-max)))

(defsubst nvp-sh-help--man-string (&optional section)
  (setq section (if section (concat "^" section) "^DESCRIPTION"))
  (nvp-help-man-string section))

;; parse 'man bash' conditional switches for '[[' and '['
(defsubst nvp-sh-help--cond-switches ()
  (nvp-help-man-switches "^CONDITIONAL EXP" "\\s-+-" "^[[:alpha:]]"))

;; -------------------------------------------------------------------
;;; Cache / Lookup

(defvar nvp-sh-help-cache (make-hash-table :test 'equal))
(defvar nvp-sh-help-conditional-cache (make-hash-table :test 'equal))

;; return help string for CMD synchronously, cache result
(defun nvp-sh-help--function-string (cmd &optional section recache)
  (or (and (not recache)
           (gethash cmd nvp-sh-help-cache))
      (sh-with-help cmd 'sync "*sh-help*"
        (prog1
            (let ((res (sh-with-bash/man cmd
                         (nvp-sh-help--builtin-string)
                         (nvp-sh-help--man-string section))))
              (puthash cmd res nvp-sh-help-cache)
              res)
          (erase-buffer)))))

;; return conditional help string
(defun nvp-sh-help--conditional-string (switch)
  (or (gethash switch nvp-sh-help-conditional-cache)
      (gethash "all" nvp-sh-help-conditional-cache)
      (progn
        (nvp-sh-help--cache-conditionals)
        (nvp-sh-help--conditional-string switch))))

;; make nvp-sh-help-conditional-cache
(defun nvp-sh-help--cache-conditionals ()
  (sh-with-help "bash" 'sync "*sh-help*"
    (let ((entries (nvp-sh-help--cond-switches)))
      (dolist (entry entries)
        ;; key by -%s if possible
        (if (string-match "^\\(-[[:alnum:]]+\\).*" (car entry))
            (puthash (match-string 1 (car entry))
                     (concat (car entry) "\n" (cdr entry))
                     nvp-sh-help-conditional-cache)
          ;; otherwise just use entry, for things like
          ;; "string = string" from man
          (puthash (car entry) (cdr entry)
                   nvp-sh-help-conditional-cache)))
      ;; make one big entry for everything
      (puthash
       "all" (mapconcat
              #'(lambda (entry) (concat (car entry) "\n" (cdr entry)))
              entries
              "\n")
       nvp-sh-help-conditional-cache))
    (erase-buffer)))

;; -------------------------------------------------------------------
;;; Additional Help
;;
;; When popup is being displayed, hitting "h" displays buffer with
;; more help for current command.
;; - For bash builtins, this is 'man "bash-builtins"' and the window is
;;   scrolled to the description of the command at point.
;; - For other commands, just call Man on command.

;;; Man sections for completing read. Not optimal, currently calls
;; man twice - once to get completions, and again to get popup.
(defsubst nvp-sh-help--man-sections (cmd)
  (sh-with-man-help cmd 'sync "*sh-help*"
    (Man-build-section-list)
    Man--sections))

;; when Man finishes, set point in MAN-BUFFER to be
;; after description of CMD
;; (for `nvp-sh-help-more-help' on bash-builtins)
(defun nvp-sh-help--Man-after-notify (man-buffer cmd)
  (save-mark-and-excursion
    (with-current-buffer man-buffer
      (goto-char (point-min))
      (catch 'done
        (while (re-search-forward
                (concat "^[ \t]*" cmd "\\_>") nil 'move)
          (forward-char -1)
          (and (eq 'Man-overstrike (get-text-property (point) 'face))
               (throw 'done nil))))
      (set-window-point (get-buffer-window man-buffer) (point))))
  (display-buffer man-buffer 'not-this-window))

;; Too much trouble trying to figure out how to make Man
;; run synchronously or redefine Man notify command
(defsubst nvp-sh-help-Man-notify (man-buffer cmd)
  (run-with-timer 0.2 nil 'nvp-sh-help--Man-after-notify man-buffer cmd))

;; -------------------------------------------------------------------
;;; Help at point
(defvar Man-notify-method)
(defun nvp-sh-help-more-help (cmd)
  (interactive)
  (sh-with-bash/man cmd
    (let ((Man-notify-method 'meek))
      (man "bash-builtins")
      (nvp-sh-help--Man-after-notify "*Man bash-builtins*" cmd))
    (man cmd)))

;; show help in popup tooltip for CMD
;; show SECTION from 'man', prompting with prefix
(defun nvp-sh-help-command-at-point (cmd &optional prompt section recache)
  (interactive (list (thing-at-point 'symbol)))
  (when cmd
    (nvp-with-toggled-tip
      (or (nvp-sh-help--function-string
           cmd
           (if prompt
               ;; FIXME: calls CMD twice
               (ido-completing-read
                "Man Section: " (nvp-sh-help--man-sections cmd) nil t)
             section)
           recache)
          (format "No help found for %s" cmd))
      :help-fn (lambda ()
                 (interactive)
                 (nvp-sh-help-more-help cmd)))))

;; display help for conditional expressions: '[[' '['
(defun nvp-sh-help-conditional (switch &optional ignore)
  (interactive
   (if (x-hide-tip)
       (list nil 'ignore)
     (list (completing-read "Switch: " nvp-sh-help-conditional-cache))))
  (when (not ignore)
    (nvp-with-toggled-tip
      (nvp-sh-help--conditional-string switch) :help-fn :none)))

;; popup help for thing at point
;; - with C-u, show help for thing directly at point
;; - with C-u C-u, prompt for 'man' section, display result in popup
;; and recache result
;; - default, determine current function (not thing-at-point)
;; and find help for that.  If in [[ ... ]] or [ ... ],
;; show help for current switch, eg. "-d", or all possible switches
;;;###autoload

(defun nvp-sh-help-at-point (arg)
  (interactive "P")
  (if (equal arg '(4))
      (call-interactively 'nvp-sh-help-command-at-point)
    (let ((cmd (nvp-sh-help-current-command)))
      (cond
       ((member cmd '("[[" "["))
        ;; return help for current switch or all if not found
        (nvp-sh-help-conditional (nvp-sh-help-conditional-switch)))
       (t
        ;; with C-u C-u prompt for 'man' section and recache
        (nvp-sh-help-command-at-point
         cmd (equal arg '(16)) nil (equal arg '(16))))))))

(provide 'nvp-sh-help)
;;; nvp-sh-help.el ends here
