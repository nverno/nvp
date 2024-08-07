;;; nvp-shell-alias.el --- Read/expand/complete shell aliases -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp-hippie)


(defvar-local nvp-local-shell-aliases nil "Directory/file local aliases.")
(put 'nvp-local-shell-aliases 'safe-local-variable 'listp)

;;;###autoload
(defun nvp-shell-load-local-aliases (&optional interactive)
  "Load any local aliases from `nvp-local-shell-aliases'."
  (interactive (list t))
  (cl-assert (derived-mode-p 'comint-mode))
  (hack-local-variables)
  (let ((proc (get-buffer-process (current-buffer))) defs aliases)
    (when (and proc (bound-and-true-p nvp-local-shell-aliases))
      (pcase-dolist (`(,alias . ,exp) nvp-local-shell-aliases)
        (push alias aliases)
        (setq defs (cons (concat "alias " alias "='" exp "';") defs)))
      (comint-send-string proc (mapconcat 'identity defs))
      (when interactive
        (message "Loaded: %s" (mapconcat 'identity aliases ", "))))))

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

;; Read aliases from bash_aliases to alist ((alias . expansion) ... )
(defun nvp-shell-read-file-aliases (file &optional merge os)
  "Read aliases from FILE."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (let (res sys win)
      (while (not (eobp))
        ;; basic check: assume it is if [[ $OS == ".+" ]]
        ;; only dealing with "Windows_NT", and doesn't
        ;; try to deal with nested ifs
        (if (looking-at
             ;; eval-when-compile
             (nvp:concat "if[^!]*\\(!\\)? *\$OS.*=="
                         "\\s-*[\"']?\\([A-Za-z_0-9]+\\)"))
            (pcase (match-string-no-properties 2)
              (`"Windows_NT"
               (setq sys (if (match-string 1) 'other 'windows)))
              (_
               (setq sys (if (match-string 1) 'windows 'other))))
          (when (search-forward "alias" (line-end-position) t)
            (and (looking-at "[ \t]*\\([^=]+\\)='\\([^']+\\)'")
                 (push (list (match-string-no-properties 1)
                             (match-string-no-properties 2))
                       (if (eq sys 'windows) win res)))))
        ;; reset OS
        (when (looking-at-p "fi")
          (setq sys nil))
        ;; next line
        (forward-line 1))
      (if merge
          ;; use all aliases regardless of system type
          (nconc res win)
        (if os
            (pcase os
              ('windows win)
              (_ res))
          res)))))


;;; Completion

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

;; Get expansion of ALIAS with optional prefix CMD from alias TABLE
(defsubst nvp-shell--get-alias (alias table &optional cmd)
  (gethash alias (or (and cmd (cdr (assoc cmd table))) (car table))))

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
                  (nvp-shell--get-alias (caddr cmd) ht (cadr cmd)))))
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
            ;; (message "status: %S" status)
            (and (or
                  ;; TODO(7/29/24): second TAB when current completion is an
                  ;; exact match should exit completion optionally expanding
                  ;; abbrev - maybe add new setting to control
                  ;; (and (eq status 'exact)
                  ;;      (eq last-command this-command)
                  ;;      (equal "	" (this-command-keys)))
                  (memq status '(sole finished)))
                 (looking-back "\\_<[^ ]+" (line-beginning-position))
                 (let ((val (funcall nvp-shell-alias-completion-table
                                     (list 'value cmd (match-string 0)))))
                   (replace-match val)
                   (when (bound-and-true-p nvp-abbrev-verbose)
                     (nvp:say "expanded alias: '%s'" cmd))))))))

;;;###autoload
(defun nvp-shell-expand-alias ()
  "Expand shell alias at/before point."
  (interactive)
  (let ((completion-at-point-functions '(nvp-shell-alias-completion-at-point))
        (completion-cycle-threshold t)
        (completion-auto-select nil)
        (completion-auto-help 'always))
    ;; (completion-help-at-point)
    (call-interactively #'completion-at-point)))

;; -------------------------------------------------------------------
;;; Hippie Expand shell aliases, eg. bash shell-expand-alias C-M-e 

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
                              (nvp-shell--get-alias abbr ht cmd))
                            (funcall nvp-shell-alias-completion-table
                                     (list 'complete cmd he-search-string)))))))))
    (while (and he-expand-list         ;remove seen strings from table
                (he-string-member (car he-expand-list) he-tried-table t))
      (setq he-expand-list (cdr he-expand-list)))
    (prog1 (not (null he-expand-list))
      (if (null he-expand-list)
          (and old (he-reset-string))
        (he-substitute-string (pop he-expand-list) t)))))

(provide 'nvp-shell-alias)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-shell-alias.el ends here
