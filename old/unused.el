;; -*- no-byte-compile: t; -*-

;; -------------------------------------------------------------------
;;; Reading conf files

(autoload 'nvp-env-substitute-vars "nvp-env")

;; return the keys parsed from config file
(defsubst nvp-conf-keys (conf)
  (cl-mapcar 'car conf))

;; return keys for default target, eg.
;; [default]
;; targets=...
(defsubst nvp-conf-default-keys (conf &optional default-name key-name)
  (split-string
   (nvp-conf-value conf (or default-name "default") (or key-name "targets"))
   "[ \t,]" 'omit " "))

(defsubst nvp-conf-location-protocol (loc)
  (pcase (car (split-string loc ":"))
    ("http" 'http)
    ("https" 'http)
    ("git" 'git)
    (_ 'unknown)))

;; browse download location of target
(defsubst nvp-conf-visit-location (conf target)
  (when-let* ((loc (nvp-conf-value conf target "loc")))
    (and (eq 'http (nvp-conf-location-protocol)) (browse-url loc))))

;; Convert config of form:
;; [target]
;; key=value
;; ...
;; into alist => ((target1 (key1 . val1) (key2 . val2)) (target2 ...))
(defun nvp-conf-read (file)
  (let (res target vals)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "^\\[\\(.*\\)\\]" nil 'move)
        (setq target (match-string 1))
        (forward-line)
        (beginning-of-line)
        (while (not (or (eobp) (eq (char-after) ?\[)))
          (when (and
                 (not (looking-at-p "\\s-*#"))
                 (looking-at "[ \t]*\\(.*\\)[ \t]*=[ \t]*\\([^#\n]+\\)[ \t]*$"))
            (push (cons (match-string 1) (substitute-env-vars (match-string 2)))
                  vals))
          (forward-line))
        (push (cons target vals) res)
        (setq vals nil)))
    res))

;; find target->value in conf if it exists, nil otherwise
(defun nvp-conf-value (conf target &optional value)
  (let ((res (cdr (cl-assoc target conf :test 'string=))))
    (if (not value)
        res
      (cdr (cl-assoc value res :test 'string=)))))

;; Read config filename lines, expanding environment variables in key-value pairs
;; key-value pairs are separated by SEPARATORS and value may be quoted
;; lines beginning with COMMMENTS regex are ignored
;; separators default to ":=" and comments default to '#'
;; Return list of (key . value) pairs
(defun nvp-config-read-file (filename &optional separators comments)
  (setq separators (regexp-quote (or separators ":=")))
  (setq comments (regexp-quote (or comments "#")))
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (let ((key-val-regex
           (concat "^\\([^" separators "\n]+\\)[" separators "]+\\([^\n]+\\)"))
          (vars))
      (while (not (eobp))
        (when (and (not (looking-at-p comments))
                   (looking-at key-val-regex))
          ;; expand enviroment variables and remove quotes from values
          (push (cons (string-trim (match-string-no-properties 1))
                      (nvp-env-substitute-vars
                       (match-string-no-properties 2) 'unquote))
                vars))
        (forward-line 1))
      vars)))

;; -------------------------------------------------------------------
;;; Netrc -- FIXME: outdated, all encrypted 

;; Regex to match machine name
(defvar nvp-netrc-machine-regex
  "\\(?:machine\\)\\s-+\\([^\n]+\\)")

;; Regex to match login and password.
(defvar nvp-netrc-login/pwd-regex
  (eval-when-compile
    (concat
     "\\(?:login\\)\\s-+\\([^\n]+\\)\n"
     "\\s-*\\(?:password\\)\\s-+\\([^\n]+\\)")))

;; Check .netrc file for login/password for machine.
;;;###autoload
(defun nvp-netrc (machine &optional location)
  (let ((netrc (or location
                   (expand-file-name ".netrc" "~")
                   (expand-file-name "_netrc" "~")))
        res)
    (if (file-exists-p netrc)
        (progn
          (with-temp-buffer
            (insert-file-contents netrc)
            (goto-char (point-min))
            (while (re-search-forward nvp-netrc-machine-regex
                                      nil t)
              (when (string= (match-string-no-properties 1) machine)
                (forward-line 1)
                (re-search-forward nvp-netrc-login/pwd-regex
                                   nil t)
                (when (and (match-string-no-properties 1)
                           (match-string-no-properties 2))
                  (setq res `(,(match-string-no-properties 1)
                              ,(match-string-no-properties 2))))))))
      (user-error "File %s doesn't exist." netrc))
    res))

;; -------------------------------------------------------------------
;;; Fonts

;;- Greek letters 
;; http://www.emacswiki.org/emacs/PrettyGreek
(defvar nvp-font-greek-alist
  `(("rangle" . ?\⟩)
    ,@(cl-pairlis '("alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta"
                    "theta" "iota" "kappa" "lambda" "mu" "nu" "xi" "omicron"
                    "pi" "rho" "sigma_final" "sigma" "tau" "upsilon" "phi" "chi"
                    "psi" "omega")
                  (mapcar
                   (lambda (x) (make-char 'greek-iso8859-7 x))
                   (number-sequence 97 121)))))

;; compose chars according to `nvp-font-greek-alist'.
(defun nvp-font-greekify ()
  (mapc
   (lambda (x)
     (let ((word (car x))
           (char (cdr x)))
       (font-lock-add-keywords
        nil
        `((,(concat "\\(^\\|[^a-za-z0-9]\\)\\(" word "\\)[a-za-z]")
	   (0 (progn
		(decompose-region
		 (match-beginning 2)
		 (match-end 2))
		nil)))))
       (font-lock-add-keywords
        nil
        `((,(concat "\\(^\\|[^a-za-z0-9]\\)\\(" word "\\)[^a-za-z]")
	   (0 (progn
		(compose-region
		 (1- (match-beginning 2))
		 (match-end 2)
		 ,char)
		nil)))))))
   nvp-font-greek-alist))

