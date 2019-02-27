;;; company-bash.el --- Completion for sourced functions in sh script -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Last modified: <2019-02-26 20:55:57>
;; URL: https://github.com/nverno/shell-tools
;; Created:  8 November 2016

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

;; Completion for local variables and sourced functions.

;;; Code:
(eval-when-compile
  (require 'cl-lib))
(require 'company)
(require 'imenu)

;; time of last index creation
(defvar-local company-bash-last-index nil)

;; cached completion candidates
(defvar-local company-bash-candidates nil)

;; return list of completion candidates
(defun company-bash-candidates ()
  (if (and (not (null company-bash-last-index))
           ;; only update if last call more than few seconds old
           (< (- (float-time) company-bash-last-index) 3))
      company-bash-candidates
    ;; otherwise, set index time and refresh cache
    (setq company-bash-last-index (float-time))
    (setq company-bash-candidates (company-bash--make-index))))

;; gather sourced files
(defun company-bash--sources ()
  (save-excursion
    (goto-char (point-min))
    (let (srcs)
      (while (re-search-forward "\\_<\\(source\\|\\.\\)\\_>" nil 'move)
        (let ((syntax (syntax-ppss)))
          ;; ignore commented out / in strings
          (and (not (nth 3 syntax))
               (not (nth 4 syntax))
               (looking-at
                (eval-when-compile
                  (concat
                   ;; quoted
                   "[ \t]*\\(?:\"\\(?1:[^\"]+\\)\\|"
                   ;; or unquoted
                   "\\(?1:[^\n\t ]+\\)\\)")))
               (push (match-string-no-properties 1) srcs))))
      srcs)))

;; imenu -> target list
(defun company-bash--imenu ()
  (ignore-errors 
    (let ((index (cdr (imenu--make-index-alist))))
      (when index
        (cl-loop for (k . v) in index
           do (put-text-property 0 1 'marker v k)
           collect k)))))

;; gather functions from current/sourced files with imenu
(defun company-bash--make-index ()
  (let ((srcs (company-bash--sources))
        (res (company-bash--imenu)))
    (when srcs
      (mapc
       #'(lambda (file)
           (setq file (substitute-in-file-name file))
           (and (file-exists-p file)
                (with-current-buffer (find-file-noselect file)
                  (and (eq major-mode 'sh-mode)
                       (setq res (nconc res (company-bash--imenu)))))))
       srcs))
    res))

;; ------------------------------------------------------------
;;; Company things

(defun company-bash--prefix ()
  (and (derived-mode-p 'sh-mode)
       (not (nth 4 (syntax-ppss)))
       (company-grab-symbol)))

(defun company-bash--annotation (candidate)
  (buffer-name (marker-buffer (get-text-property 0 'marker candidate))))

(defun company-bash--location (candidate)
  (let ((marker (get-text-property 0 'marker candidate)))
    (cons (marker-buffer marker) (marker-position marker))))

(defun company-bash--candidates (arg)
  (all-completions arg (company-bash-candidates)))

;;;###autoload
(defun company-bash (command &optional arg &rest _args)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-bash))
    (prefix (company-bash--prefix))
    (annotation (company-bash--annotation arg))
    (location (company-bash--location arg))
    (candidates (company-bash--candidates arg))
    (require-match 'never)
    (duplicates nil)
    (sorted t)))

(provide 'company-bash)
;;; company-bash.el ends here
