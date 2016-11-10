;;; nvp-util --- 

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Copyright (C) 2016, Noah Peart, all rights reserved.
;; Created:  2 November 2016

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
  (require 'cl-lib))

;; convert selected bindings to macro form and align
;;;###autoload
(defun nvp-macroify-bindings (start end)
  (interactive "r")
  (goto-char start)
  (let ((map (save-excursion
               (when (re-search-forward "\\([a-zA-Z0-9-]+\\)-map" end t)
                 (match-string-no-properties 1)))))
    (when map
      (let (binds)
        (while (re-search-forward
                "\\(\"[^\"]+\"\\))[\n\t ]*[#']*\\([a-zA-Z0-9-]+\\)" end t)
          (push (format "(%s . %s)"
                        (match-string-no-properties 1)
                        (match-string-no-properties 2))
                binds))
        (goto-char start)
        (insert (concat "(nvp-bindings \"" map "\" nil \n  "
                        (mapconcat 'identity (nreverse binds) "\n  ")
                        ")\n"))
        (goto-char start)
        (mark-sexp)
        (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)\\. ")))))

;; https://gist.github.com/haxney/3055728
;; non-nil if monospaced font
(defun nvp-font-is-mono-p (font-family)
  (let ((wind (selected-window))
        m-width l-width)
   (with-temp-buffer
     (set-window-buffer (selected-window) (current-buffer))
     (text-scale-set 4)
     (insert (propertize "l l l l l" 'face `((:family ,font-family))))
     (goto-char (line-end-position))
     (setq l-width (car (posn-x-y (posn-at-point))))
     (newline)
     (forward-line)
     (insert (propertize "m m m m m" 'face `((:family ,font-family) italic)))
     (goto-char (line-end-position))
     (setq m-width (car (posn-x-y (posn-at-point))))
     (eq l-width m-width))))

;; Display various available fonts
;; https://www.emacswiki.org/emacs/GoodFonts
;;;###autoload
(defun nvp-font-list ()
  (interactive)
  (let ((str "The quick brown fox jumps over the lazy dog ´`''\"\"1lI|¦!Ø0Oo{[()]}.,:; ")
        (font-families (cl-remove-duplicates 
                        (sort (font-family-list) 
                              #'(lambda (x y) (string< (upcase x) (upcase y))))
                        :test 'string=))
        (buff (get-buffer-create "*fonts*"))
        (inhibit-read-only t))
    (with-current-buffer buff
      (erase-buffer)
      (font-lock-mode)
      (dolist (ff (cl-remove-if-not 'nvp-font-is-mono-p font-families))
        (insert 
         (propertize str 'font-lock-face `(:family ,ff))               ff "\n"
         (propertize str 'font-lock-face `(:family ,ff :slant italic)) ff "\n"))
      (pop-to-buffer (current-buffer)))))

(provide 'nvp-util)
;;; nvp-util.el ends here
