;;; nvp-hippie-shell.el --- expand shell aliases -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-hippie)
(require 'nvp-shell)

(defun nvp-shell-read-aliases (shell-cmd regex key val)
  "SHELL-CMD is a string passed to `call-process-shell-command' to print
aliases. REGEX is used to match KEY VAL pairs that are added to a hash table."
  (let ((ht (make-hash-table :size 129 :test #'equal)))
    (with-temp-buffer
      (let ((standard-output (current-buffer)))
        (call-process-shell-command shell-cmd nil t nil)
        (goto-char (point-min))
        (while (re-search-forward regex nil 'move)
          (puthash (match-string-no-properties key)
                   (match-string-no-properties val) ht))
        ht))))

(nvp:lazy-defvar nvp-shell--alias-completion-table
  (lambda ()
    (list
     ;; for bash assume all aliases are of the form
     ;; alias ..='cd ..'
     ;; eg. the start and end with "'" that way don't have to
     ;; worry about escaped single quotes when parsing
     ;; aliases
     (nvp-shell-read-aliases
      "bash -ci alias" "^alias\\s-*\\([^=]+\\)='\\(.*\\)'$" 1 2)
     (cons
      "git"
      (nvp-shell-read-aliases "git alias" "^\\([^=]+\\)=\\(.+\\)$" 1 2)))))

(eval-when-compile
  ;; get expansion of ALIAS with optional prefix CMD from alias TABLE
  (defsubst nvp:shell--get-alias (alias table &optional cmd)
    (gethash alias (or (and cmd (cdr (assoc cmd table))) (car table)))))

(defvar nvp-shell-alias-completion-table
  (lambda (args)
    (let ((ht (nvp:lazy-val nvp-shell--alias-completion-table)))
      (setq nvp-shell-alias-completion-table
            (nvp-he-completion-table
             (lambda (cmd)
               (pcase (car cmd)
                 ('complete
                  (all-completions
                   (or (caddr cmd) "")
                   (or (cdr (assoc (cadr cmd) ht)) (car ht))))
                 ('value
                  (nvp:shell--get-alias (caddr cmd) ht (cadr cmd)))))
             #'equal))
      (funcall nvp-shell-alias-completion-table args))))

(defun nvp-shell-alias-completion-at-point ()
  (-when-let* ((cmd (thing-at-point 'shell-cmd))
               (end (point))
               (beg (save-excursion
                      (progn
                        (skip-syntax-backward "^ " (line-beginning-position))
                        (point)))))
    (list beg end
          (funcall nvp-shell-alias-completion-table
                   (list 'complete cmd (buffer-substring beg end)))
          :exit-function
          (lambda (_ status)
            (and (memq status '(sole finished))
                 (looking-back "\\_<[^ ]+" (line-beginning-position))
                 (let ((val
                        (funcall nvp-shell-alias-completion-table
                                 (list 'value cmd (match-string 0)))))
                   (replace-match val)))))))

;;;###autoload
(defun nvp-shell-expand-alias ()
  "Expand shell alias at/before point."
  (interactive)
  (let ((completion-at-point-functions
         'nvp-shell-alias-completion-at-point))
    (call-interactively #'completion-at-point)))

;; -------------------------------------------------------------------
;;; Expand shell aliases, eg. bash shell-expand-alias C-M-e 

(defvar-local nvp-he-shell-alias-beg ()
  "Returns beginning position of previous shell alias.")
(setq-default nvp-he-shell-alias-beg
              #'(lambda () (car (bounds-of-thing-at-point 'symbol))))

;;;###autoload
(defun nvp-he-try-expand-shell-alias (old)
  "Expand shell alias, like bash shell-expand-alias."
  (cl-block nil
    (unless old
      (let ((cmd (thing-at-point 'shell-cmd))
            (beg (funcall nvp-he-shell-alias-beg)))
        (and (not beg) (cl-return))
        (he-init-string beg (point))
        (when (string= cmd he-search-string) (setq cmd nil))
        (unless (he-string-member he-search-string he-tried-table)
          (setq he-tried-table (cons he-search-string he-tried-table)))
        (setq he-expand-list             ;completions from hash table
              (and (not (equal "" he-search-string))
                   (let ((ht (nvp:lazy-val nvp-shell--alias-completion-table)))
                     (delq nil
                           (mapcar
                            (lambda (abbr)
                              (nvp:shell--get-alias abbr ht cmd))
                            (funcall nvp-shell-alias-completion-table
                                     (list 'complete cmd he-search-string)))))))))
    (while (and he-expand-list         ;remove seen strings from table
                (he-string-member (car he-expand-list) he-tried-table t))
      (setq he-expand-list (cdr he-expand-list)))
    (prog1 (not (null he-expand-list))
      (if (null he-expand-list)
          (and old (he-reset-string))
        (he-substitute-string (pop he-expand-list) t)))))

(provide 'nvp-hippie-exp-shell)
;;; nvp-hippie-shell.el ends here
