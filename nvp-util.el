;;; nvp-util --- 

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
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
  (require 'nvp-macro)
  (require 'cl-lib))

;;--- Lists ----------------------------------------------------------

;; Intersection of multiple lists.
(defun nvp-list-multiple-intersection (l)
  (cond ((null l) nil)
	((null (cdr l)) (car l))
	(t (cl-intersection (car l) (nvp-list-multiple-intersection
                                     (cdr l))))))

;; Split list LST into N sublists.
(defun nvp-list-split (lst n)
  (cl-loop for i from 0 to (1- (length lst)) by n
     collect (butlast (nthcdr i lst) (- (length lst) (+ n i)))))

;;--- Fonts ----------------------------------------------------------

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

;;--- Conversion -----------------------------------------------------

(declare-function hexl-hex-char-to-integer "hexl")
(declare-function hexl-oct-char-to-integer "hexl")

;; from hexl.el
;;;###autoload
(defun nvp-hex-string-to-integer (hex-string)
  "Return decimal integer for HEX-STRING."
  (interactive "sHex number: ")
  (let ((hex-num 0))
    (while (not (equal hex-string ""))
      (setq hex-num (+ (* hex-num 16)
		       (hexl-hex-char-to-integer
                        (string-to-char hex-string))))
      (setq hex-string (substring hex-string 1)))
    (message "%d" hex-num)))

;;;###autoload
(defun nvp-octal-string-to-integer (octal-string)
  "Return decimal integer for OCTAL-STRING."
  (interactive "sOctal number: ")
  (let ((oct-num 0))
    (while (not (equal octal-string ""))
      (setq oct-num (+ (* oct-num 8)
		       (hexl-oct-char-to-integer
			(string-to-char octal-string))))
      (setq octal-string (substring octal-string 1)))
    (message "%d" oct-num)))

;;--- Save Data ------------------------------------------------------
;; recentf.el
(defun nvp-save-variable (var file &optional header coding)
  "Save the `VAR' into `FILE' with `HEADER' and `CODING'."
  (condition-case error
      (with-temp-buffer
        (erase-buffer)
        (set-buffer-file-coding-system (or coding 'emacs-mule))
        (insert (or header
                    (format-message
                     (eval-when-compile
                       (concat
                        ";; -*- mode: emacs-lisp -*-\n"
                        ";;; Automatically generated on %s.\n"))
                     (current-time-string))))
        (nvp-dump-variable var)
        (insert (concat
                 "\n;; Local " "Variables:\n"
                 ";; no-update-autoloads: t\n"
                 ";; coding: " (or coding 'emacs-mule) "\n"
                 ";; End:\n"
                 ";;; " (file-name-nondirectory file)
                 " ends here\n"))
        (write-file (expand-file-name file))
        (message "Saved %s to %s." var file)
        nil)
    (error
     (warn "%s: %s" var (error-message-string error)))))

(defun nvp-dump-variable (variable)
  "Insert a \"(setq VARIABLE value)\" in the current buffer."
  (let ((value (symbol-value variable)))
    (if (atom value)
        (insert (format "\n(setq %S '%S)\n" variable value))
      (insert (format "\n(setq %S\n      '(" variable))
      (dolist (e value)
        (insert (format "\n        %S" e)))
      (insert "\n        ))\n"))))

;;--- Processes ------------------------------------------------------

(defun nvp-start-process (cmd)
  (start-process
   cmd nil shell-file-name
   shell-command-switch
   (format "nohup 1>/dev/null 2>/dev/null %s" cmd)))

;;--- Tables ---------------------------------------------------------

;; Format alist DAT as an org table.  This alist assumes :head and
;; :rows lists. It splits :rows into number of sublists matching
;; number of colums (:head).
(defun nvp-table-alist-to-org (dat)
  (let* ((head (cdr (assoc-string "head" dat)))
         (rows (append (cdr (assoc-string "rows" dat)) ()))
         (cols (length head)))
    (append head (cons 'hline nil) (nvp-list-split rows cols) nil)))

(provide 'nvp-util)
;;; nvp-util.el ends here
