;;; company-bash.el --- Completion for sourced functions in sh script -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Last modified: <2019-07-28.04>
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

;; track candidates from sourced files
(defvar bash-source-db (make-hash-table :test 'equal))

(cl-defstruct (bash-source-dbfile
               (:constructor bash-source-make-dbfile)
               (:copier nil))
  candidates
  sources
  modtime)

;; time of last index creation
(defvar-local company-bash-last-index nil)

;; cached completion candidates
(defvar-local company-bash-candidates nil)

;; gather functions from current/sourced files with imenu
(defun company-bash--make-index ()
  (let ((srcs (bash-source--buffer-sources))
        (res (bash-source--buffer-candidates)))
    (when srcs
      (mapc
       #'(lambda (file)
           (setq file (substitute-in-file-name file))
           (and (file-exists-p file)
                (with-current-buffer (find-file-noselect file)
                  (and (eq major-mode 'sh-mode)
                       (setq res (nconc res (bash-source--buffer-candidates)))))))
       srcs))
    res))

;; return list of completion candidates
(defun company-bash-candidates ()
  (if (and (not (null company-bash-last-index))
           ;; only update if last call more than few seconds old
           (< (- (float-time) company-bash-last-index) 3))
      company-bash-candidates
    ;; otherwise, set index time and refresh cache
    (setq company-bash-last-index (float-time))
    (setq company-bash-candidates (company-bash--make-index))))

;; gather sourced files from buffer
(defun bash-source--buffer-sources ()
  (save-excursion
    (goto-char (point-min))
    ;; might not be called in a `sh-mode' buffer
    (with-syntax-table sh-mode-syntax-table
      (let (srcs)
        (while (re-search-forward "\\_<\\(source\\|\\.\\)\\_>" nil 'move)
          (let ((syntax (syntax-ppss)) file)
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
                 (setq file (expand-file-name
                             (substitute-in-file-name
                              (match-string 1))))
                 (when (file-exists-p file)
                   (push file srcs)))))
        srcs))))

;; use imenu to create candidate list from buffer
(defun bash-source--buffer-candidates ()
  (ignore-errors 
    (let* ((imenu-use-markers (buffer-file-name))
           (index (cdr (imenu--make-index-alist))))
      (when index
        (cl-loop for (k . v) in index
           collect k
           ;; do (put-text-property 0 1 'marker v (copy-sequence k))
           ;; and collect k
             )))))

;; update sources/candidates for FILE in DBFILE entry
(defun bash-source--file-update (file dbfile &optional recurse imenu-regexp)
  (or imenu-regexp (setq imenu-regexp imenu-generic-expression))
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((imenu-generic-expression (or imenu-regexp imenu-generic-expression))
           (imenu-create-index-function #'imenu-default-create-index-function)
           (srcs (bash-source--buffer-sources)))
      (setf (bash-source-dbfile-candidates dbfile) (bash-source--buffer-candidates))
      (setf (bash-source-dbfile-sources dbfile) srcs)
      (puthash file dbfile bash-source-db)
      (when recurse
        (dolist (src srcs)
          (when (not (equal src file))
             (bash-source--file-candidates src recurse imenu-regexp 'no-return)))))))

;; Get/cache candidates for FILE
;; Cache is created when empty or the file's modification time has changed
;; if RECURSE is non-nil, return candidates from all files sourced recursively
(defun bash-source--file-candidates (file &optional recurse imenu-regexp no-return)
  (let* ((attr (file-attributes file 'integer))
         (modtime (and attr (nth 5 attr)))
         (dbfile (or (gethash file bash-source-db nil) (bash-source-make-dbfile))))
    (when (not (equal modtime (bash-source-dbfile-modtime dbfile)))
      (setf (bash-source-dbfile-modtime dbfile) modtime)
      (bash-source--file-update file dbfile recurse imenu-regexp))
    (unless no-return
      (if (not recurse)
          (bash-source-dbfile-candidates (gethash file bash-source-db))
        (let (res srcs)
          (cl-labels ((build-res
                       (srcfile)
                       (let ((db (gethash srcfile bash-source-db)))
                         (setq res (append res (bash-source-dbfile-candidates db)))
                         (dolist (s (bash-source-dbfile-sources db))
                           (unless (member s srcs)
                             (push s srcs)
                             (build-res s))))))
            (build-res file))
          res)))))

(defun bash-source-candidates ()
  "List of completion targets from current buffer and all recursively \
sourced files."
  (bash-source--file-candidates
   (buffer-file-name) 'recurse imenu-generic-expression))

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
  (all-completions arg (bash-source-candidates)))

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
