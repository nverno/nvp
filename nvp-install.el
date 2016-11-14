;;; nvp-install --- 

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 13 November 2016

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-local)
  (require 'cl-lib))
(require 'nvp-macro)
(autoload 'nvp-log "nvp-log")
(autoload 'nvp-package-directory-dwim "nvp-package")

;; completing read for mode configs
(defun nvp-install-list-modes ()
  (ido-completing-read
   "Mode: "
   (mapcar (lambda (x)
             (replace-regexp-in-string "\\(nvp-\\|\\.el\\)" "" x))
           (directory-files nvp/mode nil "^[^\\.].*\\.el$"))))

;;--- Parse ----------------------------------------------------------

(defvar nvp-install-mode-patterns
  '("libs:" "optional:" "git:" "bit:" "env:" "script:"))
(setq nvp-install-mode-patterns
      (nvp-with-gnu/w32
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
  (let ((file (expand-file-name (format "nvp-%s.el" mode) nvp/mode))
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

;;--- Git installs ---------------------------------------------------

;; Install git repo into site-lisp, default to github
(defun nvp-install-git (repo &optional root)
  (let* ((git-uri (format "%s/%s" (or root "https://github.com") repo))
         (pkg (car (last (split-string repo "/"))))
         (default-directory nvp/site)
         (buff (get-buffer-create "*nvp-install*")))
    (if (file-exists-p pkg)
        (progn
          (cd pkg)
          (start-process pkg buff "git" "pull"))
      (start-process pkg buff "git" "clone" git-uri))))

;; keep list of active processes, to build after they all finish
(defvar nvp-install-active-procs nil)

;; git/bit packages the build
(defvar nvp-install-pending-dirs nil)

;; Remake site-lisp after final git install.
(defun nvp-install-git-sentinel (proc msg)
  (nvp-log "%s: %s" nil (process-name proc) msg)
  (setq nvp-install-active-procs (delq proc nvp-install-active-procs))
  (if (zerop (process-exit-status proc))
      (when (not nvp-install-active-procs)
        (nvp-install-pending-dirs))
    ;; something wrong
    (nvp-log "Not rebuilding site-lisp")
    (pop-to-buffer "*nvp-install*")))

;; build installed / updated git packages
(defun nvp-install-pending-dirs ()
  (cl-loop for p in nvp-install-pending-dirs
     do (let* ((repo (car (last (split-string p "/"))))
               (dir (expand-file-name repo nvp/site)))
          (add-to-list 'load-path dir)
          (nvp-package-directory-dwim dir)))
  (setq nvp-install-pending-dirs nil)
  (load-file nvp/auto-site)
  (nvp-log "Finished installing site-lisp.")
  (pop-to-buffer "*nvp-install*"))

;;--- On Demand ------------------------------------------------------

;;; Install dependencies on demand - when mode is first autoloaded
;;;###autoload
(cl-defmacro nvp-install-on-demand
    (&key libs optional git bit env env! script sudo choco msys
          cygwin depends)
  (declare (indent defun) (debug t))
  (when load-file-name
    (let ((file (file-name-sans-extension load-file-name)))
      `(eval-when-compile
         (unless
             (file-exists-p
              (expand-file-name (concat ,file ".elc")))
           ;;--- Dependencies ----------------------------------------
           (cl-loop for dep in ,depends
              do (nvp-install-mode dep))
           ;;--- Packages --------------------------------------------
           (cl-loop for pkg in ,libs
              if (and (package-installed-p (intern pkg))
                      (locate-library pkg))
              do (nvp-log "%s already installed" nil pkg)
              else do (nvp-log "Installing %s" nil pkg)
                (package-install (intern pkg) t))
           (cl-loop for pkg in ,optional
              do (message "Package %s" pkg)
              if (and (not (and (package-installed-p (intern pkg))
                                (locate-library pkg)))
                      (y-or-n-p
                       (format "Install optional package: %s? " pkg)))
              do (package-install (intern pkg) t))
           ;;--- Git Installs ----------------------------------------
           ;; github / bitbucket
           (setq nvp-install-pending-dirs (append ,git ,bit))
           (cl-loop for pkg in ,git
              do (let ((proc (nvp-install-git pkg)))
                   (push proc nvp-install-active-procs)
                   (set-process-sentinel proc
                                         'nvp-install-git-sentinel)))
           (cl-loop for pkg in ,bit
              do (let ((proc
                        (nvp-install-git pkg
                                         "https://bitbucket.org")))
                   (push proc nvp-install-active-procs)
                   (set-process-sentinel proc
                                         'nvp-install-git-sentinel)))
           ;;--- Environment ----------------------------------------
           ;; Permanent
           (cl-loop for (var val exec clobber) in ,env!
              do
                (nvp-log "Setting %s to %s%s" nil var val
                         (if clobber " (clobbering)"))
                (nvp-env-setenv! var val exec clobber))
           ;; FIXME: handle more than just PATH
           ;; Just adds to PATH currently 
           (cl-loop for dir in ,env
              do
                (nvp-log "Adding %s to PATH" dir)
                (nvp-env-exec-add dir))
           ;;--- Scripts ---------------------------------------------
           (cl-loop for (prog args) in ,script
              do
                (nvp-log "Running %s %S" nil prog args)
                (nvp-with-process-log
                  (apply 'start-process
                         prog "*nvp-install*" prog args)
                  :pop-on-error))
           (nvp-with-gnu
             ;; sudo commands
             (cl-loop for (action cmd) in ,sudo
                do (if (eq 'install action)
                       (nvp-ext-sudo-install cmd)
                     (nvp-ext-sudo-command nil cmd))))
           (nvp-with-w32
             ;; chocolatey
             (cl-loop for pkg in ,choco
                do (w32-shell-execute
                    "runas" "cmd.exe" (format " /c cinst -y %s" pkg)))
             ;; FIXME: msys / cygwin
             )
           ;;--- Compile ---------------------------------------------
           ;; Removes macro+contents from compiled file
           (let ((outfile (concat ,file ".elc"))
                 (infile (concat ,file ".el"))
                 (tmp-file (make-temp-name
                            (file-name-nondirectory ,file))))
             (with-temp-file tmp-file
               ;; ,@body
               (goto-char (point-max))
               (insert-file-contents infile)
               (goto-char (point-min))
               (while (search-forward "(nvp-install-on-demand" nil t)
                 (goto-char (match-beginning 0))
                 (kill-sexp 1)))
             (let ((byte-compile-dest-file-function
                    #'(lambda (_f) outfile)))
               (byte-compile-file tmp-file)
               (delete-file tmp-file))))))))

;;;###autoload
(defun nvp-install-mode (mode)
  (interactive (list (nvp-install-list-modes)))
  (load (nvp-mode mode)))

;;;###autoload
(defun nvp-install-modes (modes)
  "Install packages for list of MODES."
  (mapc #'nvp-install-mode modes))

;; -------------------------------------------------------------------

(declare-function w32-shell-execute "w32")

(provide 'nvp-install)
;;; nvp-install.el ends here
