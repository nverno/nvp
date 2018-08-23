;;; nvp-font ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 29 November 2016

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

;;--- Greek Letters --------------------------------------------------

;;; Obsolete
(defun nvp-font-lambda ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
	  (0 (progn (compose-region (match-beginning 1) (match-end 1)
				    ,(make-char 'greek-iso8859-7 107))
		    nil))))))

;; http://www.emacswiki.org/emacs/PrettyGreek
(defvar nvp-font-greek-alist
  `(("rangle" . ?\⟩)
    ,@(cl-pairlis '("alpha" "beta" "gamma" "delta" "epsilon" "zeta"
                    "eta"
                    "theta" "iota" "kappa" "lambda" "mu" "nu" "xi"
                    "omicron" "pi" "rho" "sigma_final" "sigma" "tau"
                    "upsilon" "phi" "chi" "psi" "omega")
                  (mapcar
                   (lambda (x) (make-char 'greek-iso8859-7 x))
                   (number-sequence 97 121)))))

;; compose chars according to `nvp-font-greek-alist'.
(defun nvp-font-greekify ()
  (mapc
   (lambda (x)
     (let ((word (car x))
           (char (cdr x)))
       (font-lock-add-keywords
        nil
        `((,(concat "\\(^\\|[^a-za-z0-9]\\)\\(" word "\\)[a-za-z]")
	   (0 (progn
		(decompose-region
		 (match-beginning 2)
		 (match-end 2))
		nil)))))
       (font-lock-add-keywords
        nil
        `((,(concat "\\(^\\|[^a-za-z0-9]\\)\\(" word "\\)[^a-za-z]")
	   (0 (progn
		(compose-region
		 (1- (match-beginning 2))
		 (match-end 2)
		 ,char)
		nil)))))))
   nvp-font-greek-alist))

;;--- Glyphs ---------------------------------------------------------

;;;###autoload
(defun nvp-font-quote-glyphs ()
  (let ((tbl (make-display-table)))
    (aset tbl 8220 (vector (make-glyph-code ?\" 'default)))
    (aset tbl 8221 (vector (make-glyph-code ?\" 'default)))
    (aset tbl 8216 (vector (make-glyph-code ?\` 'default)))
    (aset tbl 8217 (vector (make-glyph-code ?\' 'default)))
    (setq standard-display-table tbl)))

;;;###autoload
(defun nvp-font-glyphify (item glyph)
  `((,item
     (0 font-lock-keyword-face t)
     (0 (prog1
            (compose-region (match-beginning 0)
                            (match-end 0)
                            ,glyph)
          nil)))))

;;--- Fontify --------------------------------------------------------

;; Add face to region.
;;;###autoload
(defun nvp-font-fontify-region (beg end face)
  (interactive (list
                (region-beginning)
                (region-end)
                (read-face-name "Face")))
  (put-text-property beg end 'font-lock-face face))

;;--- Other ----------------------------------------------------------

;; https://gist.github.com/haxney/3055728
;; non-nil if monospaced font
(defun nvp-font-is-mono-p (font-family)
  (let (m-width l-width)
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
                              #'(lambda (x y) (string< (upcase x)
                                                  (upcase y))))
                        :test 'string=))
        (buff (get-buffer-create "*fonts*"))
        (inhibit-read-only t))
    (with-current-buffer buff
      (erase-buffer)
      (font-lock-mode)
      (dolist (ff (cl-remove-if-not 'nvp-font-is-mono-p font-families))
        (insert 
         (propertize str 'font-lock-face
                     `(:family ,ff)) ff "\n"
         (propertize str 'font-lock-face
                     `(:family ,ff :slant italic)) ff "\n"))
      (pop-to-buffer (current-buffer)))))

(provide 'nvp-font)
;;; nvp-font.el ends here
