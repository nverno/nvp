;;; nvp-font.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-16 13:34:45>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 29 November 2016

;;; Commentary:
;; font/glyph related functions
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))

;; -------------------------------------------------------------------
;;; Fontify 

;;;###autoload
(defun nvp-font-fontify-region-face (face &optional beg end)
  "Fontify region or `thing-at-point' with font FACE.
With \\[universal-argument] prompt for THING at point."
  (interactive
   (list
    (read-face-name "Fontifaction face: ")
    (nvp-region-or-batp (eq 4 (prefix-numeric-value current-prefix-arg)))))
  (put-text-property beg end 'font-lock-face face))

;; -------------------------------------------------------------------
;;; Display

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

;; https://www.emacswiki.org/emacs/GoodFonts
;;;###autoload
(defun nvp-font-list ()
  "Display various available fonts."
  (interactive)
  (let ((str "The quick brown fox jumps over the lazy dog \
´`''\"\"1lI|¦!Ø0Oo{[()]}.,:; ")
        (font-families (cl-remove-duplicates 
                        (sort (font-family-list) 
                              #'(lambda (x y) (string< (upcase x)
                                                  (upcase y))))
                        :test 'string=)))
    (nvp-with-results-buffer "*Fonts*"
     (font-lock-mode)
     (dolist (ff (cl-remove-if-not 'nvp-font-is-mono-p font-families))
       (insert (propertize str 'font-lock-face `(:family ,ff)) ff "\n"
        (propertize str 'font-lock-face `(:family ,ff :slant italic)) ff "\n")))))

;; -------------------------------------------------------------------
;;; Glyphs 

(defun nvp-font-quote-glyphs ()
  (let ((tbl (make-display-table)))
    (aset tbl 8220 (vector (make-glyph-code ?\" 'default)))
    (aset tbl 8221 (vector (make-glyph-code ?\" 'default)))
    (aset tbl 8216 (vector (make-glyph-code ?\` 'default)))
    (aset tbl 8217 (vector (make-glyph-code ?\' 'default)))
    (setq standard-display-table tbl)))

(defun nvp-font-glyphify (item glyph)
  `((,item
     (0 font-lock-keyword-face t)
     (0 (prog1
            (compose-region (match-beginning 0)
                            (match-end 0)
                            ,glyph)
          nil)))))

(provide 'nvp-font)
;;; nvp-font.el ends here
