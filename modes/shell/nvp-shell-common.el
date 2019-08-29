;;; nvp-shell-common.el --- shared utils/vars -*- lexical-binding: t; -*-

;;; Commentary:
;; stuff required by numerous modes
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-shell-macs "macs/nvp-shell-macs"))
(nvp-decls)

;; dont expand when prefixed by [-/_.]
(defvar nvp-shell-abbrev-re "\\(\\_<[_:\\.A-Za-z0-9/-]+\\)")

;; name of current command
(defun nvp-shell-current-command ()
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
         ((looking-back "\\(?:^\\|[^[]\\)\\(\\[+\\)[ \t]*" (line-beginning-position))
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

;;; Things-at-point

;; guess bounds from beginning of current command to end of symbol/word at point
(defun nvp-shell-bounds-of-stmt-at-point ()
  (save-excursion
    (let* ((bol (comint-line-beginning-position))
           (end (progn (skip-syntax-forward "w_\"") (point)))
           (beg (progn (nvp-shell-goto-command-start end bol) (point))))
      (cons beg end))))
(put 'shell-stmt 'bounds-of-thing-at-point #'nvp-shell-bounds-of-stmt-at-point)

;; bounds for current active shell command
(defun nvp-shell-bounds-of-cmd-at-point ()
  (save-excursion
    (goto-char (car (bounds-of-thing-at-point 'shell-stmt)))
    (skip-syntax-forward "\"")
    (bounds-of-thing-at-point 'symbol)))
(put 'shell-cmd 'bounds-of-thing-at-point #'nvp-shell-bounds-of-cmd-at-point)

(defun nvp-shell-bounds-of-variable-at-point ()
  (save-excursion
    (let ((end (progn
                 (and (eq (char-after) ?$) (forward-char 1))
                 (and (eq (char-after) ?{) (forward-char 1))
                 (skip-syntax-forward "w_") (point)))
          (_ (skip-syntax-backward "w_"))
          (beg (if (or (eq (char-before) ?$)
                       (and (eq (char-before) ?{)
                            (eq (char-before (1- (point))) ?$)))
                   (point))))
      (and beg (cons beg end)))))
(put 'shell-var 'bounds-of-thing-at-point #'nvp-shell-bounds-of-variable-at-point)

;; look for an active interactive shell process
(defun nvp-shell-get-process (&optional proc-name buffer-name)
  (cl-loop for proc in (process-list)
     when (and (process-live-p proc)
               (cond
                (proc-name (string= (process-name proc) proc-name))
                (buffer-name (string= (buffer-name (process-buffer proc))
                                      buffer-name))
                (t (process-command proc)
                   (cl-find "-i" (process-command proc) :test 'string=))))
     return proc))

(provide 'nvp-shell-common)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-shell-common.el ends here
