;;; nvp-perl.el --- perl helpers  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Other:
;; - perlcritic.el, Perl::Critic
;; - Sepia, looks like old REPL
;; - Emacs::PDE - tried it before
;;
;; TODO:
;; - Abstract cycling chars
;; - Examples:
;;   - ESS cycles assign
;;   - https://github.com/emacsmirror/cycle-quotes/blob/master/cycle-quotes.el
;; - generic parse
;; - update compile: warnings / diagnostics, input in compilation comint buffer
;;
;; FIXME:
;; - remove AC related plsense stuff
;;
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'nvp-parse))
(require 'cperl-mode)
(nvp-req 'nvp-perl 'subrs)
(nvp-auto "find-lisp" 'find-lisp-find-files)

(nvp-decls :f (auto-complete-mode
               plsense-setup-current-buffer plsense-server-start))

(nvp-with-w32
  ;; load windows environment helpers
  (add-to-list
   'load-path
   (expand-file-name "w32" (file-name-directory
                            (or load-file-name (buffer-file-name)))))
  (require 'perl-w32tools))

;;; Things at point

(defun nvp-perl--module ()
  (nvp-back-chars-then-look "_[:alpha:]:\\->" "[_[:alpha:]:]+"))

(defun nvp-perl--variable ()
  (nvp-back-chars-then-look "[:alpha:]_$@#%*&=" "[[:alpha:]_$@#%*&]+"))

(put 'perl-module 'bounds-of-thing-at-point 'nvp-perl--module)
(put 'perl-variable 'bounds-of-thing-at-point 'nvp-perl--variable)

;;; Generics

;; list modules used and imports
;; (("module" import1 import2 ...) ("module2" ... ))
(cl-defmethod nvp-parse-includes (&context (major-mode cperl-mode) &rest args)
  (nvp-parse:buffer-file 'buffer nil args
    (save-excursion
      (goto-char (point-min))
      (let (res imports)
        (while (re-search-forward
                (nvp-concat
                 ;; Module 
                 "\\<use\\>[ \t]*\\([.0-9A-Za-z:]+\\);?[ \t]*"
                 ;; Imports
                 "\\(?:qw(\\(?2:[^\)]+\\))\\|\\(?2:[^ \t]\\)\\)")
                nil 'move)
          (setq imports (match-string-no-properties 2))
          (push (cons (match-string-no-properties 1)
                      (if imports (split-string imports) nil))
                res))
        res))))

;;; Cpanm

;; Install module using cpanm
(defun nvp-perl-cpanm-install ()
  (interactive)
  (let ((module (read-from-minibuffer "Module: " (thing-at-point 'perl-module t))))
    (nvp-with-process "cpanm"
      :buffer-fn get-buffer-create
      :proc-args (module))))

;; -------------------------------------------------------------------
;;; Completion

;; FIXME: Remove AC related stuff
;; switch b/w completion backends
(defun nvp-perl-toggle-completion ()
  (interactive)
  (cond
   ((bound-and-true-p auto-complete-mode)
    (and (fboundp 'plsense-server-stop)
         (plsense-server-stop))
    (auto-complete-mode -1)
    (and (require 'company nil t)
         (company-mode)))
   ((and (bound-and-true-p company-mode)
         (fboundp 'plsense-config-default))
    (company-mode -1)
    (plsense-config-default)
    (auto-complete-mode)
    (plsense-setup-current-buffer)
    (plsense-server-start))
   (t (and (fboundp 'company-mode)
           (company-mode)))))

;;; Eldoc
(defun nvp-perl-eldoc-function ()
  (ignore-errors
    (nvp-unless-ppss 'soc
      (car
       (let ((cperl-message-electric-keyword nil))
         (cperl-get-help))))))

;; ------------------------------------------------------------
;;; Insert / Toggle

;; non-nil if point is in hash definition
(defsubst nvp-perl-hash-p ()
  (save-excursion
    (ignore-errors 
      (backward-up-list)
      (beginning-of-line)
      (looking-at-p ".*%.*="))))

(defun nvp-perl-cycle (seq)
  (nvp-cycle (kbd "<tab>") seq :exit-fn #'nvp-perl-cycle-exit))

;; hook run after 'my' abbrev toggle is done
(defun nvp-perl-cycle-exit ()
  (unless (eq this-command 'keyboard-quit)
    (let ((char (char-before)))
      (undo-boundary)
      (pcase char
        (`?% (yas-expand-snippet
              "$1${2: = (\n  ${3:x} => $4\n$0);}" nil nil
              '((yas-indent-line 'auto))))))))

;; FIXME: inserts in weird places
;; Add a new perl use statement after the existing use statements.
(defun nvp-perl-add-use (&optional module)
  (interactive
   (list
    (read-from-minibuffer "Module: " (thing-at-point 'perl-module))))
  (if (not (assoc-string module (nvp-parse-includes)))
      (save-excursion
        (goto-char (point-min))
        ;; goto end of last use declaration
        (while (re-search-forward "\\<use\\>" nil t))
        ;; if still at beginning of buffer, skip over comments
        (when (bobp)
          (forward-comment (point-max)))
        (forward-line 1)
        (insert (format "use %s%s\n" module
                        (if (string-match-p ";" module) "" ";"))))))

;; ------------------------------------------------------------
;;; Debug

;; Launch debugger
;; on windows just run in an external shell
(defun nvp-perl-debug (_arg)
  (interactive "P")
  (nvp-with-gnu/w32
      (let ((process-environment
             (cons "PERL5DB_THREADED=1" process-environment)))
        (call-interactively 'cperl-db))
    (perl-w32tools-debug-shell _arg)))

;; Insert 'use Data::Printer; p `var'' where `var' is the variable
;; near the point.  If invoked with an argument, comments out the
;; line where `var' is found.
(defun nvp-perl-insert-debug-statement (var &optional comment)
  (interactive
   (let ((var (thing-at-point 'perl-variable t)))
     (list (read-string (nvp-prompt-default "Expression to dump: " var) nil nil var)
           current-prefix-arg)))
  (when comment
    (progn
      (beginning-of-line)
      (cperl-indent-command)
      (insert "# ")))
  (save-excursion
    (unless
        (re-search-backward
         "^[[:space:]]*[^#]*[[:space:]]*use[[:space:]]+Data::Printer;" nil t)
      (goto-char (point-min))
      (nvp-perl-add-use "Data::Printer")))
  (save-excursion
    (back-to-indentation)
    (split-line)
    (insert (format "p %s;" var))
    (comment-indent)
    (insert "DEBUG")))

;; ------------------------------------------------------------
;;; Find Stuff
;; `perl-find-library' stuff

;; build perl modules paths
(nvp-define-cache-runonce nvp-perl--module-paths ()
  (cl-remove-if-not
   #'(lambda (dir)
       (and (string-match "/" dir)
            (file-exists-p dir)))
   (car
    (read-from-string
     (shell-command-to-string
      (eval-when-compile
        (concat "perl -e '$\"=\"\\\" \\\"\";"
                "print \"(\\\"@INC\\\")\"'")))))))

;; Find all perl modules in directories on @INC, and cache
;; searches for files ending in .pod or .pm and translates
;; file path separators to '::'
(nvp-define-cache-runonce nvp-perl-modules ()
  (cl-mapcan
   (lambda (dir)
     (mapcar
      (lambda (file)
        (nvp-perl-replace-all
         ;; chop suffixes
         (rx (seq "." (| "pod" "pm") string-end))
         ""
         ;; convert file path separators to '::'
         (nvp-perl-replace-all
          "/" "::" (substring file (1+ (length dir))))))
      (find-lisp-find-files
       dir (rx (seq "." (| "pod" "pm") string-end)))))
   (nvp-perl--module-paths)))

;; return path to perl module
(defun nvp-perl-module-path (module)
  (let ((path
         (shell-command-to-string (concat "perldoc -l " module))))
    (and (not (string-match-p "No documentation" path))
         (substring path 0 (1- (length path))))))

;; completing read for installed modules
(defun nvp-perl-read-module (&optional prompt default path)
  (or default (setq default (thing-at-point 'perl-module t)))
  (setq prompt (nvp-prompt-default (or prompt "Module: ") default))
  (let ((module (nvp-completing-read prompt (nvp-perl-modules))))
    (if path (nvp-perl-module-path module)
      path)))

(defun nvp-perl-jump-to-module (module)
  "Jump to MODULE in other window."
  (interactive (list (nvp-perl-read-module nil nil 'path)))
  (with-demoted-errors "Error in nvp-perl-jump-to-module: %S"
    (find-file-other-window module)))

(provide 'nvp-perl)
;;; nvp-perl.el ends here
