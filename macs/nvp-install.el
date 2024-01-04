;;; nvp-install.el --- Install configs on first load -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; It's ugly but it works...
;;
;; Rewrite:
;; - interface to makefile install
;; - defstruct mode configs
;; - generic fetcher
;; - better logging / cleanup / error reporting
;; - parallel pkg install
;; - async external installs
;;
;;; Code:

(require 'nvp-macs-common "macs/nvp-macs-common")
(require 'nvp-macs-process "macs/nvp-macs-process")
(require 'nvp-macs-decls "macs/nvp-macs-decls")
(require 'nvp-macs-setup "macs/nvp-macs-setup")

(nvp:decls)
(declare-function nvp-pkg-directory-dwim "nvp-pkg")
(autoload 'nvp-log "nvp-log")

;; possible local locations
(defvar nvp-install-local-locations '("~/.local/bin/" "/usr/local/bin/"))

(defvar nvp-install-buffer-name "*nvp-install*")

;; locate program in local locations
;;;###autoload
(defun nvp-install-find-local-program (program &optional path)
  (nvp:with-gnu/w32
      (cl-loop for p in (if path (cons path nvp-install-local-locations)
                          nvp-install-local-locations)
         do (let ((f (expand-file-name program p)))
              (and (file-exists-p f)
                   (cl-return f))))
    (bound-and-true-p (intern (concat "nvp-" program "-program")))))

;; -------------------------------------------------------------------
;;; Parse arguments

(defvar nvp-install-mode-patterns
  '("libs:" "optional:" "git:" "bit:" "env:" "script:"))
(setq nvp-install-mode-patterns
      (nvp:with-gnu/w32
          `(,@nvp-install-mode-patterns "sudo:")
        `(,@nvp-install-mode-patterns "choco:" "msys:" "cygwin:")))

;; check for continued config line
(defun nvp-install-escaped-p ()
  (save-excursion
    (goto-char (line-end-position))
    (looking-back "\\\\\\s-*" (line-beginning-position))))

(defun nvp-install-get-single-line ()
  (let ((start (progn
                 (skip-chars-forward ";* \t" (line-end-position))
                 (point)))
        (end (progn
               (goto-char (line-end-position))
               (skip-chars-backward "\\\\ \t")
               (point))))
    (split-string (buffer-substring-no-properties start end)
                  "[ \t]" t "\\s-*")))

;; get a full line allowing for escaped newlines
(defun nvp-install-parse-line ()
  (let ((acc (nvp-install-get-single-line)))
    (while (and (nvp-install-escaped-p) (not (eobp)))
      (forward-line 1)
      (nconc acc (nvp-install-get-single-line)))
    acc))

;; parse config for MODE. Returns list of matches for each pattern
;; or nil if file doesn't exist or no configs present
(defun nvp-install-get-packages (mode &optional patterns)
  (let ((file (expand-file-name (format "nvp-%s.el" mode) nvp/config))
        (case-fold-search nil)
        (patterns (or patterns nvp-install-mode-patterns)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (cl-loop for pattern in patterns
                 do (goto-char (point-min))
                 if (re-search-forward (concat "^;+[*]\\s-*" pattern) nil t)
                 do (goto-char (match-end 0))
                 and collect (cons pattern (nvp-install-parse-line)))))))

;; -------------------------------------------------------------------
;;; Git Installs 

(defsubst nvp-install-normalize-pkgnames (pkgs)
  (setq pkgs (nvp:as-list pkgs))
  (--map (string-remove-suffix ".el" (pcase it
                                       ((pred plistp) (plist-get it :repo))
                                       (_ it)))
         pkgs))

;; Install git repo into site-lisp, default to github
(defun nvp-install-git (repo &optional root)
  (let* ((repository (if (plistp repo) (plist-get repo :repo) repo))
         (git-uri (format "%s/%s" (or root "git@github.com:") repository))
         (pkg (car (nvp-install-normalize-pkgnames
                    (car (last (split-string repository "/"))))))
         (default-directory nvp/site)
         (buff (get-buffer-create nvp-install-buffer-name))
         (git-args (if (plistp repo) (list "-b" (plist-get repo :branch)))))
    (if (file-exists-p pkg)
        (progn (cd pkg)
               (start-process pkg buff "git" "pull"))
      (apply #'start-process
             (append (list pkg buff "git" "clone") git-args (list git-uri pkg))))))

;; keep list of active processes, to build after they all finish
(defvar nvp-install-active-procs nil)

;; git/bit packages the build
(defvar nvp-install-pending-dirs nil)

;; build installed / updated git packages
(defun nvp-install-pending-dirs ()
  (cl-loop for p in nvp-install-pending-dirs
     do (let* ((repo (car (last (split-string p "/"))))
               (dir (expand-file-name repo nvp/site)))
          (add-to-list 'load-path dir)
          (nvp-pkg-directory-dwim dir)))
  (setq nvp-install-pending-dirs nil)
  (load-file nvp/auto-site)
  (nvp-log "Finished installing site-lisp.\n")
  (kill-buffer nvp-install-buffer-name))

;; -------------------------------------------------------------------
;;; On Demand 

;; number of processes to wait for before compiling
(defvar nvp-install--total-proc 0)

;; push process onto active process list, log it, and delete if
;; when finished. When all active process are done, compile
(defun nvp-install-execute-process (process file)
  ;; (declare (indent 1) (debug t))
  (cl-incf nvp-install--total-proc)
  (nvp:with-process-log process
    :on-error (pop-to-buffer (current-buffer))
    :on-success (progn
                  (cl-decf nvp-install--total-proc)
                  (when (zerop nvp-install--total-proc)
                    ;; build site-lisp packages
                    (nvp-install-pending-dirs)
                    ;; compile mode
                    (nvp-install-compile file)))))

;; compile the file, removing macro+contents
(defun nvp-install-compile (file)
  (let* ((default-directory (file-name-directory file))
         (outfile (concat file ".elc"))
         (infile (concat file ".el"))
         (tmp-file (make-temp-name (file-name-nondirectory file))))
    (with-temp-file tmp-file
      (goto-char (point-max))
      (insert-file-contents infile)
      (goto-char (point-min))
      (save-match-data
        (while (search-forward "(nvp-install-on-demand" nil t)
          (goto-char (match-beginning 0))
          (let ((p (point)))
            (forward-sexp 1)
            (delete-region p (point))))))
    (let ((byte-compile-dest-file-function
           #'(lambda (_f) outfile)))
      (byte-compile-file tmp-file)
      (delete-file tmp-file))))

(defun nvp-install-site-paths (path &rest repos)
  (let ((paths
         (delq nil
               (append
                (mapcar #'eval path)
                (mapcar (lambda (r) (car (last (string-split r "/" t " ")))) repos)))))
    (cl-loop for p in paths
             as path = (cond
                        ((stringp p)
                         (if (file-name-absolute-p p) p
                           (expand-file-name p nvp/site)))
                        (t (error "unhandled path: %S" p)))
             collect path)))

(defun nvp-install--mode (mode)
  (load (nvp:mode-config-path mode)))

;;; Install dependencies on demand - when mode is first autoloaded
(cl-defmacro nvp-install-on-demand
    (&key libs optional git bit env env! script path depends patch)
  (declare (indent defun) (debug t))
  (require 'nvp-local)
  (setq path (seq-uniq
              (apply #'nvp-install-site-paths
                     (nvp:as-list (eval path))
                     (nvp:list-unquote (append (nvp-install-normalize-pkgnames git) bit)))))
  `(progn
     ,(when load-file-name
        (let ((file (file-name-sans-extension load-file-name)))
          `(eval-when-compile
             (unless
                 (file-exists-p
                  (expand-file-name (concat ,file ".elc")))
               ;; process counter
               (setq nvp-install--total-proc 0)
               ;;--- Dependencies ----------------------------------------
               (cl-loop for dep in ',depends
                        do (nvp-install--mode dep))
               ;;--- Packages --------------------------------------------
               (cl-loop for pkg in ',libs
                        if (and (package-installed-p (intern pkg))
                                (locate-library pkg))
                        do (nvp-log "%s already installed\n" nil pkg)
                        else do (nvp-log "Installing %s\n" nil pkg)
                        (package-install (intern pkg) 'dont-select))
               (cl-loop for pkg in ',optional
                        do (message "Package %s" pkg)
                        if (and (not (and (package-installed-p (intern pkg))
                                          (locate-library pkg)))
                                (y-or-n-p
                                 (format "Install optional package: %s? " pkg)))
                        do (package-install (intern pkg) 'dont-select))
               ;;--- Git Installs ----------------------------------------
               ;; github / bitbucket
               (setq nvp-install-pending-dirs
                     (append (nvp-install-normalize-pkgnames ',git) ',bit))
               (cl-loop for pkg in ',git
                        do (let ((proc (nvp-install-git pkg)))
                             (nvp-install-execute-process proc ,file)))
               (cl-loop for pkg in ',bit
                        do (let ((proc
                                  (nvp-install-git
                                   pkg "https://bitbucket.org")))
                             (nvp-install-execute-process proc ,file)))
               ;;--- Environment ----------------------------------------
               ;; Permanent
               (cl-loop for (var val exec clobber) in ',env!
                        do
                        (nvp-log "Setting %s to %s%s\n" nil var val
                                 (if clobber " (clobbering)"))
                        (nvp-env-setenv! var val exec clobber))
               ;; FIXME: handle more than just PATH
               ;; Just adds to PATH currently 
               (cl-loop for dir in ',env
                        do
                        (nvp-log "Adding %s to PATH\n" dir)
                        (nvp-env-path-add dir))
               ;;--- Scripts ---------------------------------------------
               (cl-loop for (prog args) in ',script
                        do
                        (nvp-log "Running %s %S\n" nil prog args)
                        (let ((proc (apply 'start-process
                                           prog nvp-install-buffer-name prog args)))
                          (nvp-install-execute-process proc ,file)))
               ;;--- Patch
               (cl-loop for (file patch) in ',patch
                        do
                        (let ((default-directory user-emacs-directory))
                          (nvp-install-execute-process
                           (start-process-shell-command
                            "patch"
                            nvp-install-buffer-name
                            (format
                             (concat
                              "patch -s -N -u $(find elpa -type f -name '%s') "
                              "-i patches/%s || true")
                             file patch))
                           ,file)))
               ;;--- Compile ---------------------------------------------
               ;; If not processes were spawned, then do the compile
               ;; here since no sentinels will get called
               (when (zerop nvp-install--total-proc)
                 (nvp-install-compile ,file))))))
     ,(if path `(eval-and-compile
                  (progn
                    ,@(cl-loop for p in path
                               if (file-exists-p p)
                               collect `(add-to-list 'load-path ,p)))))))

(provide 'nvp-install)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-install.el ends here
