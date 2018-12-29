;;; nvp-help ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 24 November 2016

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
  (require 'cl-lib)
  (defvar zeal-at-point-exe))
(require 'nvp)
(declare-function zeal-at-point-get-version "zeal-at-point")
(declare-function zeal-at-point "zeal-at-point")
(nvp-with-gnu
  (autoload 'nvp-ext-sudo-install "nvp-ext"))
(autoload 'ispell-get-word "ispell")
(autoload 'define-word "define-word")
(autoload 'define-word-at-point "define-word")
(autoload 'nvp-ext-run-script "nvp-ext")

;; -------------------------------------------------------------------
;;; Lookup Words

;; Define word at point, with single prefix prompt for word, 
;; with two prefix use lookup-word.
;;;###autoload
(defun nvp-help-define-word (arg)
  (interactive "p")
  (cond
   ((eq arg 4) (call-interactively  #'define-word))
   ((eq arg 16) (call-interactively #'nvp-help-lookup-word))
   (t (call-interactively           #'define-word-at-point))))

;; Lookup definintion of word at point online.
(defun nvp-help-lookup-word (word)
  (interactive (list (save-excursion (car (ispell-get-word nil)))))
  (browse-url (format "http://en.wiktionary.org/wiki/%s" word)))

;; -------------------------------------------------------------------
;;; Faces 

;; Show the name of face under point.
;;;###autoload
(defun nvp-help-font-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face)
      (message "No face at %d" pos))))

;; -------------------------------------------------------------------
;;; Docsets 

(autoload 'zeal-at-point-run-search "zeal-at-point")

(defun nvp-help-zeal-run-search (search)
  (if zeal-at-point-exe
      (if (version< "0.2.0" (zeal-at-point-get-version))
          (start-process "Zeal" nil zeal-at-point-exe search)
        (start-process "Zeal" nil zeal-at-point-exe "--query" search))
    (nvp-with-gnu/w32
        (and (y-or-n-p "Install zeal? ")
             (set-process-sentinel
              (nvp-ext-run-script
               (expand-file-name "script/install.sh" nvp--dir)
               '("install_zeal") 'sudo)
              ;; reset global key / zeal exe
              #'(lambda (p _m)
                  (when (zerop (process-exit-status p))
                    (global-set-key
                     (kbd "C-c d") 'zeal-at-point-search)
                    (setq zeal-at-point-exe
                          (executable-find "zeal"))))))
      (and (y-or-n-p "Zeal not found, goto http://zealdocs.org? ")
           (browse-url "http://zealdocs.org")))))

;;;###autoload
(defun nvp-help-zeal-at-point (&optional edit-search)
  (interactive "P")
  (cl-letf (((symbol-function 'zeal-at-point-run-search)
             'nvp-help-zeal-run-search))
    (zeal-at-point edit-search)))

(provide 'nvp-help)
;;; nvp-help.el ends here
