;;; company-perl --- static company completion -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/perl-tools
;; Package-Requires: 
;; Copyright (C) 2016, Noah Peart, all rights reserved.
;; Created:  6 November 2016

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

;; Uses perldoc-cache.pl from Emacs::PDE to generate cache of perl
;;  objects.

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (defvar company-perl-cache-pl)
  (defvar company-perl-cache-el)
  (defvar company-perl-program)
  (defvar perldoc-obarray))
(require 'company)
(require 'perl-tools)

(defvar company-perl-modes '(cperl-mode perl-mode pod-mode))

;; ------------------------------------------------------------

(when load-file-name
  (setq company-perl-cache-pl
        (expand-file-name "tools/perldoc-cache.pl" perl-tools--dir))
  (setq company-perl-cache-el
        (expand-file-name "tools/perldoc-cache" perl-tools--dir))
  (setq company-perl-program "perl"))

;;; From Emacs::PDE to build cache

;; Perl module names and functions
(defvar company-perl-obarray nil)

;; true when obarray is built
(defvar company-perl--enabled nil)

;; Build perldoc obarray from cache.
;; With prefix arguments force cache update.
(defun company-perl-build-obarray (&optional re-cache)
  (interactive "P")
  (if (and (null re-cache)
           (file-exists-p company-perl-cache-el))
      (load company-perl-cache-el)
    (message "Building perldoc-cache.el ...")
    (set-process-sentinel
     (start-process "perldoc-build" nil company-perl-program
                    company-perl-cache-pl company-perl-cache-el)
     #'(lambda (proc event)
         (if (zerop (process-exit-status proc))
             (progn
               (message "Created perldoc cache successfully!")
               (load company-perl-cache-el)
               (setq company-perl--enabled t))
           (error "Failed to create perldoc cache! %s" event))))))

;; recache if last cache time was more than DAYS ago
(defun company-perl-recache (&optional days)
  (let ((recache-p (and (file-exists-p company-perl-cache-el)
                        (< (time-to-seconds
                            (time-subtract
                             (current-time)
                             (nth 5 (file-attributes
                                     company-perl-cache-el))))
                           (* 60 60 24 (or days 1))))))
    (company-perl-build-obarray recache-p)))

;; ------------------------------------------------------------
;;; Company stuff

(defun company-perl-grab-symbol ()
  (if (looking-at-p "\\_>")
      (buffer-substring-no-properties
       (point) (save-excursion
                 (skip-chars-backward ":_A-Za-z")
                 (point)))
    (unless (and (char-after)
                 (memq (char-syntax (char-after)) '(?w ?_)))
      (message "TODO"))))

(defun company-perl--prefix ()
  (and (memq major-mode company-perl-modes)
       company-perl--enabled
       (not (company-in-string-or-comment))
       (company-perl-grab-symbol)))

(defun company-perl--location (candidate)
  (let ((path (perl-tools-module-path candidate)))
    (if path
        (cons (find-file-noselect path) 1)
      (message "Only know how to jump to modules."))))

(defun company-perl--candidates (arg)
  (if company-perl--enabled
      (all-completions arg perldoc-obarray)
    (company-perl-recache nil)))

;;;###autoload
(defun company-perl (command &optional arg &rest _args)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-perl))
    (prefix (company-perl--prefix))
    (candidates (company-perl--candidates arg))
    (location (company-perl--location arg))
    (require-match 'never)
    (sorted t)))

;; load / cache on load
(company-perl-recache nil)

(provide 'company-perl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; company-perl.el ends here
