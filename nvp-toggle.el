;;; nvp-toggle-*- lexical-binding: t;-*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 20 March 2017

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

;; (defun nvp-toggle-increment-numbers (start end)
;;   "Increment numbers in region. Temporarily sets 'i' to replay command.
;; Decrement with prefix."
;;   (interactive "r")
;;   (let (deactivate-mark)
;;     (goto-char start)
;;     (while (re-search-forward "\\([[:digit:]]+\\)" end 'move)
;;       (replace-match (number-to-string
;;                       (+ (if current-prefix-arg -1 1)
;;                          (string-to-number (match-string 1))))
;;                      nil nil nil 1))
;;     ;; FIXME: what is good way to reused the current-prefix-argument value when
;;     ;; calling this-command?
;;     (nvp-basic-temp-binding "i" this-command)))

;; In/De-crement numbers in region,  decremnent with prefix argument
;;;###autoload
(defun nvp-toggle-increment-numbers (start end)
  "Simple function to increment numbers in region. Decrement with prefix."
  (interactive "r")
  (let (deactivate-mark)
    (setq start  (copy-marker start)
          end    (copy-marker end))
    (goto-char start)
    (while (re-search-forward "\\([-]?[[:digit:]]+\\)" end 'move)
      (replace-match (number-to-string (+ (if current-prefix-arg -1 1)
                                          (string-to-number (match-string 1))))
                     nil nil nil 1))
    (setq prefix-arg current-prefix-arg)
    (set-transient-map
     (let ((km (make-sparse-keymap)))
       (define-key km "i" this-command)
       km)
     t)))

;; -------------------------------------------------------------------
;;; Toggle local variables

;; insert new local delimeter, put point at first position
(defsubst nvp-toggle-local-insert-delm ()
  (insert "-*-  -*-")
  (forward-char -4)
  (list :start (1- (point)) :end (1+ (point))))

;; write the vars to the buffer
(defsubst nvp-toggle-local-insert (vars)
  (insert
   (mapconcat (lambda (x) (concat (car x) (and (cdr x) ": ") (cadr x))) vars "; ")))

;; return non-nil on match
(defsubst nvp-toggle-local-goto-or-insert ()
  (goto-char (point-min))
  (let ((start (search-forward "-*-" (line-end-position) 'move)))
    (if (not start)
        (nvp-toggle-local-insert-delm)
      (search-forward "-*-" (line-end-position) 'move)
      (prog1 (list :start start :end (- (point) 3))
        (goto-char start)))))

;; -*- a b: 1 -*-
;; get local vars -- which may be key-value pairs
(defun nvp-toggle-local-vars ()
  (let ((p (point))
        (end (progn (and (search-forward "-*-" (line-end-position) 'move)
                         (- (point) 3))))
        lst)
    (when end
      (goto-char p)
      (while (re-search-forward
              (nvp-concat "\\s-*\\([-a-zA-Z0-9]+\\)\\s-*"
                          "\\(:\\s-*\\([-a-zA-Z0-9]+\\)\\)?")
              end 'move)
        (if (match-string 3)
            (push (cons (match-string 1) (match-string 3)) lst)
          (and (match-string 1)
               (push (cons (match-string 1) nil) lst)))))
    lst))

;; insert key val combo into local header
(defun nvp-toggle-local-add (input &optional remove)
  (interactive (list (read-string "Key-val(sep [ :]): ")
                     current-prefix-arg))
  (let* ((pos (nvp-toggle-local-goto-or-insert))
         (vars (nvp-toggle-local-vars))
         (vals (split-string input "[: ]" 'omit " "))
         (res (cl-delete-if (lambda (x) (string= (car x) (car vals))) vars)))
    (delete-region (plist-get pos :start) (plist-get pos :end))
    (nvp-toggle-local-insert
     (if remove res (cons (cons (car vals) (cdr vals)) res)))))

;;;###autoload
(defun nvp-toggle-local-binding (local-var com-start com-end)
  (interactive
   (list (read-string "Binding: " "mode: ")
         (or comment-start (read-string "Comment start: " "/* "))
         (or (and comment-start comment-end) (read-string "Comment end: " " */"))))
  (let ((parts (split-string local-var ":" 'omit " ")))
    (save-excursion
      (goto-char (point-min))
      (cond
       ((not (looking-at-p (regexp-quote com-start))) ;no starting comment
        (insert (format "%s -*- %s -*- %s\n" com-start local-var com-end)))
       ((and (car parts)                   ;already defined -- update or toggle off
             (looking-at-p (concat ".*" (regexp-quote (car parts)) ".*$")))
        (when (re-search-forward
               (concat "\\(" (regexp-quote (car parts))
                       "\\s-*:\\s-*\\([A-Za-z0-9-]*\\s-*\\);?\\)")
               (line-end-position))
          (if (not (cadr parts))
              (replace-match "" nil nil nil 1)                         ;toggle off
            (replace-match (concat (cadr parts) " ") nil nil nil 2)))) ;update
       (:else
        (if (not (search-forward "-*-" (line-end-position) 'move))
            (insert (format " -*- %s -*- " local-var))
          (insert (format " %s; " local-var))))))))

(provide 'nvp-toggle)
;;; nvp-toggle.el ends here
