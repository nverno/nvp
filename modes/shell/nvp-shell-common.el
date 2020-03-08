;;; nvp-shell-common.el --- shared utils/vars -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-proc))
(nvp-req 'nvp-shell 'subrs)
(nvp-decls)

;; dont expand when prefixed by [-/_.]
(defvar nvp-shell-abbrev-re "\\(\\_<[_:\\.A-Za-z0-9/-]+\\)")

;;; Interop variables for eshell, and other non-comint-based shells
;; used for determining the shell statement and active command around point
(defvar nvp-shell-bol-function #'comint-line-beginning-position)
(defvar nvp-shell-command-delimiters "^)(|&\`;\[")

;; name of current command in sh scripts
(defun nvp-sh-current-command ()
  (let ((ppss (parse-partial-sexp (point-min) (point)))
        (start (or (cdr (bounds-of-thing-at-point 'symbol)) (point))))
    (when (not (nth 4 ppss)) ;; ignore comments
      (save-excursion
        (catch 'done
          (while t
            (skip-chars-backward "^:<>)(|&\`;\[" (line-beginning-position))
            (if (or (not (nth 3 (syntax-ppss)))  ; not in string
                    (eq (char-before) ?\`)       ; in old-school subcommand
                    (and (eq (char-before) ?\()  ; at start of variable
                         (eq (char-before (1- (point))) ?$)))
                (throw 'done nil)
              ;; move backward out of enclosing string that shouldn't be a quoted
              ;; command
              (up-list -1 t t))))
        (skip-syntax-forward " " start)
        (cond
         ;; '[[' or '['
         ((looking-back "\\(?:^\\|[^[]\\)\\(\\[+\\)[ \t]*" (line-beginning-position))
          (match-string 1))
         ;; 'if' => if in situation like 'if ! hash', then
         ;; return 'hash'
         ((looking-at-p "if\\_>")
          (if (looking-at "if[ \t]+!?[ \t]*\\([-+[:alnum:]]+\\)")
              (match-string 1)
            "if"))
         ;; otherwise, return first symbol, which may be incorrect, eg. "then"
         ;; or "done", this is half-ass parse-job
         (t (and (looking-at "[:+_\[\.[:alnum:]-]+")
                 (match-string 0))))))))

;; -------------------------------------------------------------------
;;; Things-at-point

;; guess bounds from beginning of current command to end of symbol/word at point
(defun nvp-shell-bounds-of-statement ()
  (save-excursion
    (let* ((bol (funcall nvp-shell-bol-function))
           (end (progn (skip-syntax-backward " ")
                       (skip-syntax-forward "w_\"")
                       (point)))
           (beg (progn
                  (nvp-shell-goto-command-start end bol
                    nvp-shell-command-delimiters)
                  (point))))
      (and (> end beg) (cons beg end)))))
(put 'shell-stmt 'bounds-of-thing-at-point #'nvp-shell-bounds-of-statement)

;; bounds for current active shell command
(defun nvp-shell-bounds-of-command ()
  (--when-let (nvp-shell-bounds-of-statement)
    (save-excursion
      (goto-char (car it))
      (skip-syntax-forward "\"")
      (bounds-of-thing-at-point 'symbol))))
(put 'shell-cmd 'bounds-of-thing-at-point #'nvp-shell-bounds-of-command)

;; (defun nvp-shell-bounds-of-variable-at-point ()
;;   (save-excursion
;;     (let ((end (progn
;;                  (and (eq (char-after) ?$) (forward-char 1))
;;                  (and (eq (char-after) ?{) (forward-char 1))
;;                  (skip-syntax-forward "w_") (point)))
;;           (_ (skip-syntax-backward "w_"))
;;           (beg (if (or (eq (char-before) ?$)
;;                        (and (eq (char-before) ?{)
;;                             (eq (char-before (1- (point))) ?$)))
;;                    (point))))
;;       (and beg (cons beg end)))))
;; (put 'shell-var 'bounds-of-thing-at-point #'nvp-shell-bounds-of-variable-at-point)

;; -------------------------------------------------------------------
;;; Processes

(defun nvp-shell-get-process (&optional buff-name proc-name sh-name)
  "Return a live and interactive shell process or nil."
  (let ((interactive-shell-p
         ;; if a specific shell is supplied, find a match that was also started
         ;; interactively, otherwise just look for an interactive (hopefully) shell
         (if sh-name (lambda (cmd)
                       (and (string= sh-name (file-name-base (car cmd)))
                            (cl-find "-i" (cdr cmd) :test #'string=)))
           (lambda (cmd) (cl-find "-i" cmd :test #'string=))))
        (buff-name (if (eq t buff-name) (buffer-name (current-buffer))
                     buff-name)))
   (nvp-proc-find-if
     (lambda (p)
       (when (process-live-p p)
         (or (and proc-name (string= proc-name (process-name p)))
             (and buff-name (string= buff-name (buffer-name (process-buffer p))))
             (funcall interactive-shell-p (process-command p))))))))

(provide 'nvp-shell-common)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-shell-common.el ends here
