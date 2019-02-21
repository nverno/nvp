;;; nvp-vc.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-21 03:20:25>
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
  (require 'cl-lib))
(require 'nvp)
(require 'nvp-string)

;; checkout part of a repo
;;;###autoload
(defun nvp-vc-git-sparse-clone (repo subdir local-dir)
  "Clone SUBDIR from REPO to LOCAL-DIR."
  (interactive
   (let* ((repo (read-string "Repository URI: "))
          (subdir (read-string "Subdirectory to clone: "))
          (ddir (car (last (split-string repo "/"))))
          (local-dir (read-directory-name "Clone into: "
                                          (or (getenv "DEVEL")
                                              default-directory)
                                          ddir nil ddir)))
     (list repo subdir local-dir)))
  (unless (file-exists-p local-dir)
    (mkdir local-dir))
  (let ((default-directory local-dir))
    (call-process-shell-command
     (format "git init && git remote add -f origin %s && \
git config core.sparseCheckout true" repo) nil nil nil)
    (with-current-buffer (find-file-noselect ".git/info/sparse-checkout")
      (goto-char (point-max))
      (insert subdir)
      (pop-to-buffer-same-window (current-buffer))
      (add-hook 'after-save-hook
                #'(lambda ()
                    (let ((default-directory `,local-dir))
                      (start-process "git" nil "git" "pull" "origin" "master")))
                nil 'local))))

;; -------------------------------------------------------------------
;;; SVN 

;; Cached list of git svn subcommands
(nvp-function-with-cache nvp-vc-svn--available-commands ()
  :doc "List of git svn subcommands."
  (nvp-s-all-matches
   "^  \\([-a-z]+\\) +" (shell-command-to-string "git svn help") 1))

;;;###autoload
(defun nvp-vc-svn (dir command)
  "Run a git svn subcommand in `DIR'."
  (interactive (list (read-directory-name "Directory: " nil default-directory)
                     (nvp-completing-read
                      "Git svn command to run: "
                      (nvp-vc-svn--available-commands)
                      nil t nil nil)))
  (let* ((default-directory (vc-git-root dir))
         (compilation-buffer-name-function
          #'(lambda (_major-mode-name) "*git-svn*")))
    (compile (concat "git svn " command))))

;; -------------------------------------------------------------------
;;; Magit 

(defsubst nvp-vc-magit-ref ()
  (interactive)
  (browse-url "https://magit.vc/manual/magit-refcard.pdf"))

(provide 'nvp-vc)
;;; nvp-vc.el ends here
