;;; w32-registry --- major mode for editing windows registry files  -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/w32-tools
;; Package-Requires: 
;; Created:  8 November 2016

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

;;  `w32-registry-mode' is major mode for editing windows registry files.
;;  Adds some convenience functions to look at registry key values, basic
;;  menu, bindings, imenu, etc.
;;  Company completion for registry values
;;  [company-w32reg](http://github.com/nverno/company-w32reg)

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'pp))

(defgroup w32-registry nil
  "Simple mode for windows registry files."
  :group 'languages
  :prefix "w32-registry-")

;; ------------------------------------------------------------
;;* variables
(defcustom w32-registry-header
  "Windows Registry Editor Version 5.00"
  "Header to insert in '.reg' file."
  :group 'w32-registry
  :type 'string)

(defcustom w32-registry-exe
  (or (executable-find "reg.exe")
      (expand-file-name "system32/reg.exe" (getenv "windir")))
  "Location of 'reg.exe' executable."
  :group 'w32-registry
  :type 'string)

(defcustom w32-registry-regedit-exe
  (or (executable-find "regedit.exe")
      (expand-file-name "regedit.exe" (getenv "windir")))
  "Location of regedit.exe executable."
  :group 'w32-registry
  :type 'string)

(defcustom w32-registry-regex
  "\\([A-Za-z_0-9]+\\)\\s-*\\(REG_[A-Za-z]+\\)\\s-*\\([0-9a-zA-Z]+\\)"
  "Regex to match registry entries: (subkey, type, value)."
  :group 'w32-registry
  :type 'regex)

(defcustom w32-registry-auto-header t
  "If non-nil, automatically insert `w32-registry-template' in empty
buffers."
  :group 'w32-registry
  :type 'boolean)


;; ------------------------------------------------------------
;;; Major-mode

(defvar w32-registry-menu
  '("Registry"
    ["Lookup" w32-registry-template :help "Show registry key in buffer" t]
    ["List" w32-registry-list :help "Convert registry key contents to list" t]
    ["Value" w32-registry-value :help "Show value of subkey" t]
    "--"
    ["Regedit" w32-registry-regedit :help "Open regedit.exe" t]
    "--"
    ["Template" w32-registry-template :help "Insert template" t]))

(defvar w32-registry-mode-map
  (let ((map (make-sparse-keymap)))
    (easy-menu-define nil map nil w32-registry-menu)
    (define-key map (kbd "C-c C-t") #'w32-registry-template)
    (define-key map (kbd "C-c C-?") #'w32-registry-lookup)
    (define-key map (kbd "C-c C-l") #'w32-registry-list)
    (define-key map (kbd "C-c C-v") #'w32-registry-value)
    (define-key map (kbd "C-c C-e") #'w32-registry-regedit)
    map))

;;;###autoload
(define-derived-mode w32-registry-mode conf-windows-mode "Reg"
  "Major mode for editing windows registry files.

\\{w32-registry-mode-map}"
  (setq-local local-abbrev-table w32-registry-mode-abbrev-table)
  (setq-local imenu-generic-expression '((nil "^\\[\\([^\]]+\\)\\].*" 1)))
  (setq-local imenu-case-fold-search t)
  (setq-local case-fold-search t)
  (setq-local outline-regexp "[^\]]")
  (when (and w32-registry-auto-header
             (= 0 (buffer-size (current-buffer))))
    (w32-registry-template)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.reg$" . w32-registry-mode))


;; ------------------------------------------------------------
;;; Commands

(defun w32-registry-regedit ()
  "Open regedit.exe externally."
  (interactive)
  (call-process-shell-command w32-registry-regedit-exe nil 0))

(defun w32-registry-lookup (key)
  "Show value of registry KEY in buffer."
  (interactive "sRegKey: ")
  (let ((buff (get-buffer-create "*w32-registry*")))
    (with-output-to-temp-buffer buff
      (call-process w32-registry-exe nil buff t "query" key))))

(defun w32-registry-list (key)
  "Lookup KEY in registry, return list of form ((subkey type value)) or nil if 
no entries/error."
  (interactive "sRegKey: ")
  (let (res)
    (with-temp-buffer
      (call-process w32-registry-exe nil t nil "query" key)
      (goto-char (point-min))
      (while
          (re-search-forward w32-registry-regex nil t)
        (push `(,(match-string-no-properties 1)
                ,(match-string-no-properties 2)
                ,(match-string-no-properties 3))
              res))
      res)))

(defun w32-registry-value (key subkey)
  "Get SUBKEY from KEY in the registry.  Returns list of form (subkey type value)."
  (interactive "sRegKey: \nsSubkey: ")
  (let* ((subkeys (w32-registry-list key))
         (res (cl-find subkey subkeys
                       :test (lambda (x y) (string= (downcase x) (downcase (car y)))))))
    (when (called-interactively-p 'any)
      (pp res))
    res))

(defun w32-registry-template ()
  "Insert header for '.reg' file.  If buffer is empty, template is 
inserted and point moved to end of buffer (ie. if in `conf-windows-mode-hook'),
otherwise template is inserted at beginning of buffer and point preserved."
  (interactive)
  (let ((size (buffer-size (current-buffer))))
    (save-excursion
      (goto-char (point-min))
      (insert w32-registry-header)
      (insert "\n"))
    (when (= 0 size) (goto-char (point-max)))))

(provide 'w32-registry-mode)
;;; w32-registry.el ends here
