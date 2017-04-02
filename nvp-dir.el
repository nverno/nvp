;;; nvp-dir ---

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created:  2 December 2016

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
  (require 'nvp-macro)
  (require 'subr-x)
  (require 'cl-lib)
  (require 'hydra nil t))
(require 'dired)
(autoload 'nvp-ext-terminal "nvp-ext")
(autoload 'dired-filename-at-point "dired-x")

;; -------------------------------------------------------------------
;;; Dired

;;;###autoload
(defun nvp-dired-jump (&optional other-window file-name)
  (interactive
   (list nil (and current-prefix-arg
                  (read-file-name "Jump to Dired file: "))))
  (global-set-key (kbd "C-x C-j") #'dired-jump)
  (dired-jump other-window file-name))

;;; Movement

(defsubst nvp-dired-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))

(defsubst nvp-dired-back-to-top ()
  (interactive)
  (widen)
  (goto-char (point-min))
  (forward-line 2)
  (nvp-dired-start-of-files))

(defsubst nvp-dired-jump-to-bottom ()
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1)
  (recenter-top-bottom))

(defsubst nvp-dired-find-alternate-file ()
  (interactive)
  (find-alternate-file ".."))

(defsubst nvp-dired-next5 ()
  (interactive)
  (forward-line 5)
  (dired-move-to-filename))

(defsubst nvp-dired-prev5 ()
  (interactive)
  (forward-line -5)
  (dired-move-to-filename))

(defsubst nvp-dired-scroll-up ()
  (interactive)
  (scroll-up-command)
  (dired-move-to-filename))

(defsubst nvp-dired-scroll-down ()
  (interactive)
  (scroll-down-command)
  (dired-move-to-filename))

(defhydra nvp-dired-hydra (:color red :hint nil)
  "move"
  ("M-n" nvp-dired-next5)
  ("M-p" nvp-dired-prev5)
  ("n" nvp-dired-next5)
  ("p" nvp-dired-prev5)
  ("i" nvp-dired-scroll-down)
  ("k" nvp-dired-scroll-up)
  ("l" dired-prev-dirline)
  ("j" dired-next-dirline))

;;; Kill
;; copy absolute filenames as string to kill ring
;; with prefix, separate with ', ', otherwise ' '
(defun nvp-dired-kill (arg)
  (interactive "P")
  (let ((files (condition-case nil
                   (or (dired-get-marked-files)
                       (cons (dired-filename-at-point) nil))
                 (error nil)))
        string)
    (when files
      (setq string (mapconcat 'identity files (if arg ", " " ")))
      (if (eq last-command 'kill-region)
          (kill-append string nil)
        (kill-new string)))))

;; -------------------------------------------------------------------
;;; External

(eval-when-compile
  (defvar nvp-dired-external-filelist-cmd)
  (defvar nvp-dired-external-program))

;;; Open externally

;; Open current or marked dired files in external app.
(defun nvp-dired-external-open ()
  (interactive)
  (let ((files (or (dired-get-marked-files)
                   (dired-file-name-at-point))))
    (nvp-with-gnu/w32
        (mapc (lambda (path)
                (let ((process-connection-type nil))
                  (start-process "" nil "xdg-open" path)))
              files)  
      (mapc (lambda (path)
              (w32-shell-execute "open" (w32-long-file-name path)))
            files))))

;; Open directory in gui
(defun nvp-dired-external-explorer ()
  (interactive)
  (nvp-with-gnu/w32
      (let ((process-connection-type nil)
            (prog (if (file-exists-p "/usr/bin/gvfs-open")
                      "/usr/bin/gvfs-open"
                    "/usr/bin/xdg-open")))
        (start-process "" nil prog "."))
    (w32-shell-execute "open" default-directory)))

;; Open file[s] in external program 
(defun nvp-dired-external ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (dolist (file files)
      (when-let ((prog (assoc (file-name-extension file)
                              nvp-dired-external-program)))
        (let ((cmd (cdr (assoc 'cmd prog))))
          (nvp-with-gnu/w32
              (start-process cmd nil cmd file)
            (w32-shell-execute (cdr (assoc 'cmd prog)) file)))))))

;;; Install info
(autoload 'org-texinfo-export-to-info "ox-texinfo")

;; Convert marked .org files to .info and install.  If marked files
;; are already .info files, just install.
;; installs files in current directory, or prompts with prefix
(defun nvp-dired-convert-and-install-info (arg)
  (interactive "P")
  (let* ((info-dir
          (replace-regexp-in-string
           "/+$" ""
           (expand-file-name
            (if arg (read-directory-name
                     "Install info to directory: ")
              default-directory))))
         (keep (or (not arg)
                   (string= info-dir default-directory))))
    ;; convert org files to .info
    (mapc (lambda (f)
            (when (string= "org" (file-name-extension f))
              (with-current-buffer (find-file-noselect f)
                (org-texinfo-export-to-info)
                (kill-buffer (current-buffer))))
            (let* ((base (file-name-nondirectory (file-name-sans-extension f)))
                   (info (concat base ".info"))
                   (dest (expand-file-name info info-dir)))
              ;; install .info files, delete intermediates, copy to destination
              (when (file-exists-p info)
                (delete-file (concat base ".texi"))
                (copy-file info dest t)
                ;; delete .info file if .org exists and moving to different
                ;; info directory
                (and (not keep) 
                     (file-exists-p (concat base ".org"))
                     (delete-file info))
                (call-process "install-info" nil (nvp-process-buffer) nil
                              (concat "--info-dir=" info-dir)
                              (concat "--info-file=" dest)))))
          (dired-get-marked-files))))

;;; Compress

(defun nvp-dired-zip ()
  (interactive)
  (let* ((files (dired-get-marked-files))
         (out-file (if (or current-prefix-arg (> (length files) 1))
                       (read-from-minibuffer "Archive name: ")
                     (concat (file-name-nondirectory (car files))
                             ".zip")))
         (in-files (mapconcat #'(lambda (x)
                                  (let ((file
                                         (file-name-nondirectory x)))
                                    (if (file-directory-p x)
                                        (concat file "/*")
                                      file)))
                              files " ")))
    (if  (= 1 (length files))
        (let* ((file (file-name-nondirectory (car files))))
          (start-process "compress" "*compress*" "7za" "a" "-tzip"
                         out-file in-files))
      (start-process-shell-command
       "compress" "*compress*" (format "7za a -tzip %s %s"
                                       out-file in-files)))))

;;; Shell

(defun nvp-dired-shell-here ()
  "Open an `shell' in current directory."
  (interactive)
  (let* ((current-dir (dired-current-directory))
        (str
         (if (file-remote-p current-dir)
             (let ((v (tramp-dissect-file-name current-dir t)))
               (format "ssh %s@%s\n"
                       (aref v 1) (aref v 2)))
           (format "cd \"%s\"\n" current-dir))))
    (process-send-string
     (nvp-ext-terminal) str)
    (setq default-directory current-dir)))

;;; Process

(defun nvp-dired-start-process (cmd &optional file-list)
  "Call shell command CMD on FILE-LIST."
  (interactive
   (let ((files (dired-get-marked-files
                 t current-prefix-arg)))
     (list
      (nvp-with-gnu
        (dired-read-shell-command "& on %s: "
                                  current-prefix-arg files))
      files)))
  (nvp-with-gnu/w32
      (let (list-switch)
        (start-process
         cmd nil shell-file-name
         shell-command-switch
         (format
          "nohup 1>/dev/null 2>/dev/null %s \"%s\""
          (if (and (> (length file-list) 1)
                   (setq list-switch
                         (cadr
                          (assoc cmd
                                 nvp-dired-external-filelist-cmd))))
              (format "%s %s" cmd list-switch)
            cmd)
          (mapconcat #'expand-file-name file-list "\" \""))))
    (dolist (file file-list)
      (w32-shell-execute "open" (expand-file-name file)))))

;;; FIXME: rsync

;; (defun nvp-dired-rsync (dest)
;;   (interactive
;;    (list (expand-file-name
;;           (read-file-name "Rsync to:" (dired-dwim-target-directory)))))
;;   ;; store all selected files into "files" list
;;   (let ((files (dired-get-marked-files nil current-prefix-arg))
;;         ;; The rsync command
;;         ()
;;         )))

;; -------------------------------------------------------------------
;;; Imenu

;; Find the previous file in the buffer.
(defsubst nvp-dired-imenu-prev-index-position ()
  (dired-previous-line 1))

;; Get name of file at point.
(defsubst nvp-dired-imenu-extract-index-name ()
  (dired-get-filename 'verbatim))

;; Configure imenu for current dired buffer.
(defun nvp-dired-setup-imenu ()
  (set (make-local-variable 'imenu-prev-index-position-function)
       #'nvp-dired-imenu-prev-index-position)
  (set (make-local-variable 'imenu-extract-index-name-function)
       #'nvp-dired-imenu-extract-index-name))

;; -------------------------------------------------------------------

(declare-function tramp-dissect-file-name "tramp")
(declare-function dired-read-shell-command "dired-aux")
(declare-function w32-shell-execute "w32")

(declare-function hydra-default-pre "hydra")
(declare-function hydra-keyboard-quit "hydra")
(declare-function hydra--call-interactively-remap-maybe "hydra")
(declare-function hydra-show-hint "hydra")
(declare-function hydra-set-transient-map "hydra")

(provide 'nvp-dir)
;;; nvp-dir.el ends here
