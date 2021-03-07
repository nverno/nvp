;;; nvp-env.el --- environment management -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(require 'nvp-log)
(declare-function w32-shell-execute "w32")
(declare-function nvp-read-with-message "nvp-read")
(nvp-auto "env" 'substitute-env-vars 'read-envvar-name 'setenv)

(defun nvp-env-substitute-vars (string &optional unquote)
  "Substitute environment variables in STRING.
If UNQUOTE is non-nil remove surrounding quotes.  Result is trimmed of surrounding
whitespace either way."
  (setq string (substitute-env-vars string 'leave-undefined))
  (if unquote
      (string-trim string "[\" \t\n\r]+" "[\" \t\n\r]+")
    (string-trim string)))

(eval-when-compile
 ;; split environment variable, remove duplicates maintaining ordering
 (defsubst nvp-env-split-var (var)
   (when-let ((env (getenv var)))
     (split-string env path-separator)))

 ;; remove duplicate entries, maintaining ordering
 (defsubst nvp-env-uniq (parts)
   (cl-remove-duplicates
    parts
    :test (nvp-with-gnu/w32 'string= '(lambda (x y) (string= (downcase x) (downcase y))))
    :from-end t))

 ;; join env var with path-separator
 (defsubst nvp-env-join (parts)
   (mapconcat 'identity parts path-separator)))

;; move entries in environment VAR matching REGEX to front of list
(defun nvp-env-rearrange (var regex &optional case-fold)
  "Move entries in env VAR matching REGEX to front of list and return new list.
Optionally ignore case with CASE-FOLD.  Preserve existing order of entries."
  (let ((parts (nvp-env-split-var var))
        (case-fold-search case-fold)
        hits misses)
    (when parts
      (mapc (lambda (s)
              (if (string-match-p regex s)
                  (push s hits)
                (push s misses)))
            parts)
      (nconc (nreverse hits) (nreverse misses)))))

(defun nvp-env-merge (var1 var2)
  "Merge entries of two environment variables."
  (nvp-env-join
   (nvp-env-uniq (append (nvp-env-split-var var1) (nvp-env-split-var var2)))))

(defun nvp-env-setenv (env-var value &optional append)
  "Set ENV-VAR to VALUE or APPEND the new VALUE."
  (if (not append)
      (setenv env-var value)
    (setenv env-var
            (nvp-env-join
             (nvp-env-uniq (cons value (nvp-env-split-var env-var)))))))

(defun nvp-env-append (env-var value)
  "Append VALUE to ENV-VAR."
  (nvp-env-setenv env-var value 'append))

;;;###autoload
(defun nvp-env-add (env-var value &optional test-string)
  "Add VALUE to ENV-VAR if it isn't present already.
Check if TEST-STRING is present in ENV-VAR if non-nil.
If ENV-VAR is nil, set with new VALUE."
  (if-let ((env (getenv env-var)))
      (and (not (string-match-p (regexp-quote (or test-string value)) env))
           (setenv env-var (nvp-env-join (list value env))))
    (setenv env-var value)))

;; -------------------------------------------------------------------
;;; Interactive

;;;###autoload
(defun nvp-env-set-var (env-var value &optional clobber)
  "Add VALUE to ENV-VAR interactively.
With prefix, overwrite value instead of appending by default."
  (interactive
   (let* ((var (read-envvar-name "Environment variable: " t))
          (current-val (getenv var)))
     (list var (nvp-read-with-message "Value: " "Current: %s" current-val)
           (y-or-n-p "Clobber current value? "))))
  (if clobber (setenv env-var value)
    (nvp-env-add env-var value))
  (message "%s=%s" env-var (getenv env-var)))

;; ------------------------------------------------------------
;;; PATH

;;;###autoload
(define-obsolete-function-alias 'nvp-add-exec-path 'nvp-env-path-add nil)
;;;###autoload
(define-obsolete-function-alias 'nvp-env-exec-add 'nvp-env-path-add nil)
;; Append DIR to environment variable PATH and EXEC-PATH.
;;;###autoload
(defun nvp-env-path-add (dir)
  "Append DIR to PATH and `exec-path'."
  (let ((path (nvp-env-uniq (cons dir (nvp-env-split-var "PATH")))))
    (setenv "PATH" (nvp-env-join path))
    (setq exec-path path)))

(defun nvp-env-rearrange-process-path (regex &optional case-fold)
  "Rearrange process environment path by REGEX.
Return `process-environment' with new value tacked on front (first is used)."
  (cons (concat "PATH=" (nvp-env-uniq (nvp-env-rearrange "PATH" regex case-fold)))
        process-environment))

;; ------------------------------------------------------------
;;; Setenv

;;;###autoload (autoload 'nvp-env-setenv! "nvp-env")
(nvp-with-w32
  ;; Update registry value ENV-VAR by setting/appending VALUE (user).
  ;; add to exec-path if EXEC is non-nil. If CLOBBER is non-nil,
  ;; overwrite variable if it already exists.
  (defun nvp-env-setenv! (env-var value &optional exec clobber &rest ignore)
    (let* ((ps (expand-file-name "script/Set-Env.ps1" nvp--dir))
           (val (replace-regexp-in-string "/" "\\\\" value)))
      (w32-shell-execute "runas" "powershell"
                         (format " -File %s \"%s\" \"%s\" %s"
                                 ps env-var val (if clobber "-Clobber" "")))
      ;; update env for current session as well
      (when exec
        (if (string= (upcase env-var) "PATH")
            (nvp-env-path-add val)
          (setenv env-var val))))))

(nvp-with-gnu
  (defun nvp-env-setenv! (env-var value &optional exec clobber append)
    (let ((current (getenv env-var)))
      (when (or clobber append (not current))
        ;; when appending, check it is already present first
        (if append
            (and (not (string-match-p (regexp-quote value) current))
                 (setenv env-var (concat current path-separator value)))
          ;; either no current value or clobbering it
          (setenv env-var value))))
    (when exec
      (if (string= (upcase env-var) "PATH")
          (nvp-env-path-add value)))))

;; ------------------------------------------------------------
;;; Windows Search Path

;;;###autoload (autoload 'nvp-env-update-path-db "nvp-env")
(nvp-with-w32
  ;; update executable database, rerun build script
  ;; defaults to $APPDATA/exe-index on windows
  (defun nvp-env-update-path-db (&optional path)
    (nvp-log "Updating executable database")
    (set-process-sentinel
     (apply #'start-process
            "updatedb" nvp-log-buffer "powershell"
            `("-f" ,(expand-file-name "Update-ExecDB.ps1" nvp/binw)
              ,@path))
     #'nvp-env-update-path-db-sentinel))

  (defun nvp-env-update-path-db-sentinel (p m)
    (nvp-log (format "%s: %s" (process-name p) m))
    (when (zerop (process-exit-status p))
      (nvp-log "DB Update completed"))))

(provide 'nvp-env)
;;; nvp-env.el ends here
