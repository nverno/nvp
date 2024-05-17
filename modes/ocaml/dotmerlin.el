;;; dotmerlin.el --- Major mode for .merlin files    -*- lexical-binding: t; -*-

;; Copyright (c) 2014 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL:
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "24"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode for .merlin files.  See: <URL:https://github.com/the-lambda-church/merlin/wiki/project-configuration>

;;; Code:

(defgroup dotmerlin nil
  "Dotmerlin configuration file."
  :group 'languages
  :prefix "dotmerlin-")

(defface dotmerlin-option
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for dotmerlin declarations."
  :group 'dotmerlin)

(defvar dotmerlin-mode-lock-keywords
  `((,(rx line-start (* space)
          (group symbol-start
                 (or "S" "B" "PKG" "REC" "EXT" "PRJ" "FLG" "CMI" "CMT")
                 symbol-end)
          (* space) (group (one-or-more not-newline)))
     (1 'dotmerlin-option)
     (2 font-lock-variable-name-face)))
  "Font lock keywords for `dotmerlin-mode'.")

(defvar dotmerlin-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?#  "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `dotmerlin-mode'.")

;;;###autoload
(define-derived-mode dotmerlin-mode conf-mode "Dotmerlin"
  "Major mode for editing .merlin files.

\\{dotmerlin-mode-map}"
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")
  (set (make-local-variable 'font-lock-defaults) '(dotmerlin-mode-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.merlin\\'" . dotmerlin-mode))

(provide 'dotmerlin)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; dotmerlin.el ends here
