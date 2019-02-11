;;; nvp-auto.el --- lesser used autos but bound to keys -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-10 18:41:27>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Maintainer: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created:  2 February 2019

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
  (require 'cl-lib)
  (require 'subr-x)
  (require 'nvp-macro)
  (require 'hydra))
(require 'nvp)
(nvp-declare "nadvice" advice-mapc advice-remove)
(declare-function helm-show-kill-ring "")

;;; FIXME: remove / update macroify bindings
;;; TODO:
;; - remove all methods from generic

;; -------------------------------------------------------------------
;;; Movement
;; see `paragraph-start' and `paragraph-separate' to extend

;;;###autoload
(defun nvp-move-down-paragraph (&optional arg)
  (interactive "p")
  (if (bolp)
      (progn
        (forward-paragraph arg)
        (forward-line 1))
    (line-move arg)))

;;;###autoload
(defun nvp-move-up-paragraph (&optional arg)
  (interactive "p")
  (if (bolp)
      (progn
        (forward-line -1)
        (backward-paragraph arg)
        (forward-line 1))
    (line-move (- arg))))

;;;###autoload(autoload 'nvp-hydra-goto-line/goto-line "nvp-auto")
(nvp-hydra-set-property 'nvp-hydra-goto-line)
(defhydra nvp-hydra-goto-line (goto-map) "line"
  ("g" goto-line "go")
  ("b" (push-mark (car mark-ring) nil 'activate) "mark to start")
  ("m" set-mark-command "mark" :bind nil)
  ("p" (set-mark-command 1) "pop" :bind nil)
  ("e" exchange-point-and-mark "exchange")
  ("q" nil "quit"))

;; -------------------------------------------------------------------
;;; Yank / Pop

;;; FIXME: how to disable hints in minibuffer
;;;###autoload(autoload 'nvp-hydra-yank-pop/yank-pop "nvp-auto")
;;;###autoload(autoload 'nvp-hydra-yank-pop/yank "nvp-auto")
(nvp-hydra-set-property 'nvp-hydra-yank-pop)
(defhydra nvp-hydra-yank-pop ()
  "yank"
  ("C-y" yank nil)
  ("M-y" yank-pop nil)
  ("y" (yank-pop 1) "next")
  ("Y" (yank-pop -1) "prev")
  ("l" helm-show-kill-ring "list" :color blue))

;; -------------------------------------------------------------------
;;; Random

;; https://emacs.stackexchange.com/questions/24657/unadvise-a-function-remove-all-advice-from-it
;;;###autoload
(defun nvp-advice-remove-all (sym)
  "Remove all advice from SYM."
  (interactive "aFunction: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

;; Print counts of strings in region, with prefix dump at point
;;;###autoload
(defun nvp-stats-uniq (beg end &optional count-lines)
  "Print counts (case-insensitive) of unique words in region BEG to END.
With prefix COUNT-LINES count unique lines."
  (interactive "r\nP")
  (require 'nvp-hash)
  (let ((ht (make-hash-table :test 'case-fold))
        (lines (split-string
                (buffer-substring-no-properties beg end) "\n" 'omit-nulls " "))
        lst)
    (if count-lines
        (dolist (line lines)
          (puthash line (1+ (gethash line ht 0)) ht))
      ;; strip punctuation for words
      (cl-loop for line in lines
         as words = (split-string line "[[:punct:] \t]" 'omit " ")
         when words
         do (cl-loop for word in words
               do (puthash word (1+ (gethash word ht 0)) ht))))
    (maphash (lambda (key val) (push (cons val key) lst)) ht)
    (setq lst (cl-sort lst #'> :key #'car))
    (nvp-with-results-buffer nil
      (pcase-dolist (`(,k . ,v) lst)
        (princ (format "%d: %s\n" k v))))))

;; convert selected bindings to macro form and align
;;;###autoload
(defun nvp-macroify-bindings (start end)
  (interactive "r")
  (goto-char start)
  (let ((map (save-excursion
               (when (re-search-forward "\\([a-zA-Z0-9-]+\\)-map"
                                        end t)
                 (match-string-no-properties 1)))))
    (when map
      (let (binds)
        (while (re-search-forward
                "\\(\"[^\"]+\"\\))?[\n\t ]*[#']*\\([a-zA-Z0-9-]+\\)"
                end t)
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

(provide 'nvp-auto)
;;; nvp-auto.el ends here
