;;; nvp-vc ---  -*- lexical-binding: t; -*-

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
  (require 'cl-lib))
(autoload 'nvp-string-all-matches "nvp-util")

;; checkout part of a repo
;;;###autoload
(defun nvp-vc-git-sparse-clone (repo subdir local-dir)
  "Clone SUBDIR from REPO to LOCAL-DIR."
  (interactive
   (let* ((repo (read-string "Repository URI: "))
          (subdir (read-string "Subdirectory to clone: "))
          (ddir (car (last (split-string repo "/"))))
          (local-dir (read-directory-name "Clone into: "
                                          (or (getenv "LOCALSRC")
                                              default-directory)
                                          ddir nil ddir)))
     (list repo subdir local-dir)))
  (unless (file-exists-p local-dir)
    (mkdir local-dir))
  (let ((default-directory local-dir))
    (call-process "git" nil nil nil "init")
    (call-process "git" nil nil nil "remote" "add" "-f" "origin" repo)
    (call-process "git" nil nil nil "config" "core.sparseCheckout" "true")
    (with-current-buffer (find-file-noselect ".git/info/sparse-checkout")
      (goto-char (point-max))
      (insert subdir)
      (pop-to-buffer-same-window (current-buffer))
      (add-hook 'after-save-hook
                #'(lambda ()
                    (let ((default-directory `,local-dir))
                      (start-process "git" nil "git" "pull" "origin" "master")))
                nil 'local))))

;;--- Magit ----------------------------------------------------------

(defsubst nvp-vc-magit-ref ()
  (interactive)
  (browse-url "https://magit.vc/manual/magit-refcard.pdf"))

;;--- SVN ------------------------------------------------------------

;; Cached list of git svn subcommands
(defvar nvp-vc-svn--available-commands nil)
(defun nvp-vc-svn--available-commands ()
  (or nvp-vc-svn--available-commands
      (setq nvp-vc-svn--available-commands
            (nvp-string-all-matches
             "^  \\([-a-z]+\\) +"
             (shell-command-to-string "git svn help") 1))))

;;;###autoload
(defun nvp-vc-svn (dir command)
  "Run a git svn subcommand in `DIR'."
  (interactive (list (read-directory-name "Directory: ")
                     (completing-read
                      "nvp-vc-svn command: "
                      (nvp-vc-svn--available-commands)
                      nil t nil nil
                      (nvp-vc-svn--available-commands))))
  (let* ((default-directory (vc-git-root dir))
         (compilation-buffer-name-function
          #'(lambda (_major-mode-name) "*git-svn*")))
    (compile (concat "git svn " command))))

(provide 'nvp-vc)
;;; nvp-vc.el ends here
