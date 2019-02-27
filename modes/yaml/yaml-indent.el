;;; yaml-indent.el --- better yaml indentation -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/yaml-tools
;; Last modified: <2019-02-25 04:30:59>
;; Created: 2 November 2016

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

;; TODO:

;; - Continuation after '|'
;; - appveyor
;; - capf

;;; Code:
(defvar yaml-indent-offset 2
  "Default indetation offset.")

(defvar yaml-indent-cont-offset 2
  "Additional offset for continued item entries.")

;; non-nil if current line is block opener
(defun yaml-indent-block-p ()
  (save-excursion
    (back-to-indentation)
    (unless (looking-at-p "^\\s-*$")
      (skip-chars-forward "^ \t\n\r")
      (eq ?: (char-before (point))))))

;; non-nil if item line starting with '-'. If item then starts with
;; 'if' or 'for' signals :sh, otherwise :item
(defun yaml-indent-item-p ()
  (save-excursion
    (back-to-indentation)
    (when (eq ?- (char-after (point)))
      (if (progn
            (forward-char)
            (looking-at-p "\\s-*\\(?:if\\|for\\)"))
          :sh
        :item))))

;; continued item
(defun yaml-indent-item-cont-p ()
  (unless (or (yaml-indent-block-p)
              (yaml-indent-item-p))
    (let (done res)
      (save-excursion
        (beginning-of-line)
        (while (and (not done)
                    (not (bobp)))
          (forward-line -1)
          (setq res (yaml-indent-item-p))
          (setq done (or res (yaml-indent-block-p)))))
      res)))

(defun yaml-indent-bash-end-p ()
  (and (eq :sh (yaml-indent-item-cont-p))
       (save-excursion
         (back-to-indentation)
         (looking-at-p "\\(?:fi\\|done\\)"))))

;; find indentation of previous block ending with a ':', ie
;; |env:
(defun yaml-indent-previous-block-indent ()
  (save-excursion
    (while (and (not (bobp))
                (not (yaml-indent-block-p)))
      (forward-line -1))
    (current-indentation)))

(defun yaml-indent-calculate-indent ()
  (let ((prev (yaml-indent-previous-block-indent)))
    (cond
     ((or (= 1 (point-at-bol)) (yaml-indent-block-p))
      prev)
     ((yaml-indent-item-p)
      (+ prev yaml-indent-offset))
     (t
      (pcase (yaml-indent-item-cont-p)
        (:sh (if (yaml-indent-bash-end-p)
                   (+ (* 2 yaml-indent-offset) prev)
                 (+ (* 2 yaml-indent-offset) yaml-indent-cont-offset prev)))
        (:item (+ yaml-indent-offset prev))
        (_ (+ yaml-indent-offset prev)))))))

;; line indentation function, diable interactive when region non-nil
(defun yaml-indent-indent-line (&optional region dedent)
  (interactive)
  (let ((ci (current-indentation))
        (need (yaml-indent-calculate-indent)))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (if (and (not region) (or dedent (eq last-command this-command)))
          (if (/= ci 0)
              (indent-to (* (/ (- ci 1) yaml-indent-offset) yaml-indent-offset))
            (indent-to (+ yaml-indent-offset need)))
        (indent-to need)))
    (if (< (current-column) (current-indentation))
        (forward-to-indentation 0))))

(defun yaml-indent-dedent-line ()
  "De-indent current line."
  (interactive "*")
  (when (and (not (bolp)) (= (current-indentation) (current-column)))
    (yaml-indent-indent-line nil 'dedent)
    t))

;; indent region, disable interactive
(defun yaml-indent-indent-region (start end)
  (interactive "r")
  (save-excursion
    (setq end (copy-marker end))
    (goto-char start)
    (let ((pr (unless (minibufferp)
                (make-progress-reporter "Indenting region..." (point) end))))
      (while (< (point) end)
        (or (and (bolp) (eolp))
            (yaml-indent-indent-line t))
        (forward-line 1)
        (and pr (progress-reporter-update pr (point))))
      (and pr (progress-reporter-done pr))
      (move-marker end nil))))

(provide 'yaml-indent)
;;; yaml-indent.el ends here
