;;; ruby-hacks.el --- provide features for ruby-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>
;; Copyright (C) 2008 Chris Wanstrath <chris@ozmm.org>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>
;;          Chris Wanstrath <chris@ozmm.org>

;; Keywords: ruby rails languages oop

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Code:
(require 'ruby-mode)

;; other stuff

;; (defun ruby-newline-and-indent ()
;;   (interactive)
;;   (newline)
;;   (smie-indent-line))

;;;###autoload
(defun ruby-toggle-string<>simbol ()
  "Easy to switch between strings and symbols."
  (interactive)
  (let ((initial-pos (point)))
    (save-excursion
      (when (looking-at "[\"']") ;; skip beggining quote
        (goto-char (+ (point) 1))
        (unless (looking-at "\\w")
          (goto-char (- (point) 1))))
      (let* ((point (point))
             (start (skip-syntax-backward "w"))
             (end (skip-syntax-forward "w"))
             (end (+ point start end))
             (start (+ point start))
             (start-quote (- start 1))
             (end-quote (+ end 1))
             (quoted-str (buffer-substring-no-properties start-quote end-quote))
             (symbol-str (buffer-substring-no-properties start end)))
        (cond
         ((or (string-match "^\"\\w+\"$" quoted-str)
              (string-match "^\'\\w+\'$" quoted-str))
          (setq quoted-str (substring quoted-str 1 (- (length quoted-str) 1)))
          (kill-region start-quote end-quote)
          (goto-char start-quote)
          (insert (concat ":" quoted-str)))
         ((string-match "^\:\\w+$" symbol-str)
          (setq symbol-str (substring symbol-str 1))
          (kill-region start end)
          (goto-char start)
          (insert (format "'%s'" symbol-str))))))
    (goto-char initial-pos)))

;; FIXME: rails-ruby-command, el4r-ruby-eval => just evaluate, capital-word-p
(nvp:decl el4r-ruby-eval capital-word-p inf-ruby-mode)
(defvar rails-ruby-command)
(defvar inf-ruby-first-prompt-pattern)
(defvar inf-ruby-prompt-pattern)
(require 'inf-ruby nil t)

(defun run-ruby-in-buffer (buf script &optional params)
  "Run CMD as a ruby process in BUF if BUF does not exist."
  (let ((abuf (concat "*" buf "*")))
    (when (not (comint-check-proc abuf))
      (set-buffer (make-comint buf rails-ruby-command nil script params)))
    (inf-ruby-mode)
    (make-local-variable 'inf-ruby-first-prompt-pattern)
    (make-local-variable 'inf-ruby-prompt-pattern)
    (setq inf-ruby-first-prompt-pattern "^>> "
          inf-ruby-prompt-pattern "^>> ")
    (pop-to-buffer abuf)))

(defun complete-ruby-method (prefix &optional maxnum)
  (if (capital-word-p prefix)
      (let* ((cmd "x = []; ObjectSpace.each_object(Class){|i| x << i.to_s}; x.map{|i| i.match(/^%s/) ? i.gsub(/^%s/, '') : nil }.compact.sort{|x,y| x.size <=> y.size}")
             (cmd (if maxnum (concat cmd (format "[0...%s]" maxnum)) cmd)))
        (el4r-ruby-eval (format cmd prefix prefix)))
    (save-excursion
      (goto-char (- (point) (+ 1 (length prefix))))
      (when (and (looking-at "\\.")
                 (capital-word-p (word-at-point))
                 (el4r-ruby-eval (format "::%s rescue nil" (word-at-point))))
        (let* ((cmd "%s.public_methods.map{|i| i.match(/^%s/) ? i.gsub(/^%s/, '') : nil }.compact.sort{|x,y| x.size <=> y.size}")
               (cmd (if maxnum (concat cmd (format "[0...%s]" maxnum)) cmd)))
          (el4r-ruby-eval (format cmd (word-at-point) prefix prefix)))))))


(provide 'ruby-hacks)
