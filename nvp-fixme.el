;;; nvp-fixme.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-09 08:48:38>
;; Package-Requires: 
;; Created: 30 November 2016

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

(eval-and-compile (defvar nvp-fixme-keywords "\\<\\(TODO\\|FIXME\\):"))
(nvp-defvar nvp-fixme-font-lock-words
  `((,nvp-fixme-keywords 1 'font-lock-warning-face prepend)))

;; collect occurences of fixme keywords in buffer
(defvar-local fixme--occurences nil)
(defun fixme-collect-occurences ()
  (or (and (not (buffer-modified-p))
           fixme--occurences)
      (setq fixme--occurences
            (save-excursion
              (goto-char (point-min))
              (let (res)
                (while (re-search-forward nvp-fixme-keywords nil 'move)
                  ;; only store locations inside of comments
                  (and (nth 4 (syntax-ppss))
                       (push (line-number-at-pos) res)))
                res)))))

;; -------------------------------------------------------------------
;;; Commands 

(defun fixme-search (&optional back)
  (let ((case-fold-search t)
        (search-fn (if back 're-search-backward 're-search-forward))
        (beg (point)))
    (condition-case nil
        (progn
          (when (looking-at-p nvp-fixme-keywords)
            (forward-line (and back -1)))
          (funcall search-fn nvp-fixme-keywords)
          (goto-char (match-beginning 0)))
      (error (message "No more fixmes") (goto-char beg)))))

(defun fixme-next ()
  (interactive)
  (fixme-search))

(defun fixme-previous ()
  (interactive)
  (fixme-search 'back))

(defun fixme-occur ()
  (interactive)
  (occur nvp-fixme-keywords))

;; -------------------------------------------------------------------
;;; Mode 

(defvar fixme-menu
  '("Fixme"
    ["Next" fixme-next t]
    ["Previous" fixme-previous t]
    ["Occur" fixme-occur t]
    "---"
    ["Turn off" fixme-mode t]))

(defvar fixme-mode-map
  (let ((km (make-sparse-keymap)))
    (easy-menu-define nil km nil fixme-menu)
    (define-key km (kbd "M-s-n") 'fixme-next)
    (define-key km (kbd "M-s-p") 'fixme-previous)
    (define-key km (kbd "M-s-o") 'fixme-occur)
    km))

;;;###autoload
(define-minor-mode fixme-mode "Fixme"
  nil
  :lighter " Fixme"
  :keymap fixme-mode-map
  (if fixme-mode
      (font-lock-add-keywords nil nvp-fixme-font-lock-words)
    (font-lock-remove-keywords nil nvp-fixme-font-lock-words))
  (font-lock-flush)
  (font-lock-ensure))

(provide 'nvp-fixme)
;;; nvp-fixme.el ends here
