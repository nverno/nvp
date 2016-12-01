;;; nvp-fixme --- 

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
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

(defvar nvp-fixme-keywords "\\_<\\(?:TODO\\|FIXME\\)\:\\_>")
(defvar nvp-fixme-font-lock-words
  `((,nvp-fixme-keywords (1 font-lock-warning-face))))

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

;;--- Interactive ----------------------------------------------------

(defun fixme-next ()
  (interactive)
  (when (re-search-forward nvp-fixme-keywords)))

(defun fixme-previous ()
  (interactive)
  (when (re-search-backward nvp-fixme-keywords)))

(defun fixme-occur ()
  (interactive)
  (occur nvp-fixme-keywords))

;;--- Mode -----------------------------------------------------------
;; FIXME:

(defvar fixme-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "S-M-n") 'fixme-next)
    (define-key km (kbd "S-M-p") 'fixme-previous)
    km))

;;;###autoload
(define-minor-mode fixme-mode "Fixme"
  nil
  :lighter " Fix"
  :keymap fixme-mode-map
  (if fixme-mode
      (font-lock-add-keywords nil nvp-fixme-font-lock-words 'append)
    (font-lock-remove-keywords nil nvp-fixme-font-lock-words))
  (font-lock-flush)
  (font-lock-ensure))

(provide 'nvp-fixme)
;;; nvp-fixme.el ends here
