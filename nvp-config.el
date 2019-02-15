;;; nvp-config.el --- config file helpers -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/config-tools
;; Last modified: <2019-02-15 10:50:29>
;; Package-Requires: 
;; Created:  10 November 2016

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
(autoload 'nvp-env-substitute-vars "nvp-env")

;;;###autoload
(defun nvp-config-xev ()
  "Run xev with output to emacs buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*xev*")
    (pop-to-buffer (current-buffer))
    (local-set-key (kbd "C-c C-c") 'kill-this-buffer)
    (nvp-with-process "xev"
      :buffer-fn get-buffer-create
      :proc-filter nil
      :on-success (kill-buffer))))

;;;###autoload
(defun nvp-config-jump-to-dotfile (action)
  "Jump to dotfile.
With single prefix, open in this window.
With double prefix, set coding to utf-8."
  (interactive (list (car current-prefix-arg)))
  (let* ((default-directory nvp/dots)
         (ido-default-file-method (if (eq 4 action) 'raise-frame 'other-window))
         (buff (if (eq 4 action)
                   (ido-find-file)
                 (ido-find-file-other-window))))
    (when (eq 16 action)
      (with-current-buffer buff
        (set-buffer-file-coding-system 'utf-8-unix nil t)))))

;; ------------------------------------------------------------
;;; Config files

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

(provide 'nvp-config)

;;; nvp-config.el ends here
