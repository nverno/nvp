;;; nvp-outline.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-01-31 20:01:27>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; Maintainer: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created: 31 January 2019

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
  (require 'hydra))
(require 'hydra)
(require 'outline)

;;;###autoload
(defun nvp-outline-add-locals ()
  "Add outline local variables for elisp headers."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (insert
     (concat
      "\n;;; Local\svariables:\n"
      ";;; outline-minor-mode: t\n"
      (format ";;; outline-regexp: \"%s\"\n"
              (regexp-quote ";;\\*\\|;;;\\*\\|(\\(?:cl-\\)?def[cuvm]\\|(setq"))
      ";;; End:"))))

;;;###autoload(autoload 'nvp-outline-hydra/body "nvp-outline")
(defhydra nvp-outline-hydra (:color red :hint t)
  "
^Hide^             ^Show^           ^Move
^^^^^^------------------------------------------------------
_h_: body          _a_: all         _u_: up
_o_: other         _e_: entry       _n_: next visible
_c_: entry         _i_: children    _p_: previous visible
_l_: leaves        _k_: branches    _f_: forward same level
_d_: subtree       _s_: subtree     _b_: backward same level
"
  ;; Hide
  ("h" outline-hide-body)         ; Hide everything but headings (all body lines)
  ("o" outline-hide-other)        ; Hide other branches
  ("c" outline-hide-entry)        ; Hide this entry's body
  ("l" outline-hide-leaves)       ; Hide body lines in this entry and sub-entries
  ("d" outline-hide-subtree)      ; Hide everything in this entry and sub-entries
  ;; Show
  ("a" outline-show-all)          ; Show (expand) everything
  ("e" outline-show-entry)        ; Show this heading's body
  ("i" outline-show-children)     ; Show this heading's immediate child sub-headings
  ("k" outline-show-branches)     ; Show all sub-headings under this heading
  ("s" outline-show-subtree)      ; Show (expand) everything in this heading & below
  ;; Move
  ("u" outline-up-heading)                ; Up
  ("n" outline-next-visible-heading)      ; Next
  ("p" outline-previous-visible-heading)  ; Previous
  ("f" outline-forward-same-level)        ; Forward - same level
  ("b" outline-backward-same-level)       ; Backward - same level
  ("q" nil "quit"))
(hydra-set-property 'nvp-outline-hydra :verbosity 1)

(provide 'nvp-outline)
;;; nvp-outline.el ends here

;;; Local variables:
;;; outline-minor-mode: t
;;; outline-regexp: ";;\\\*\\|;;;\\\*\\|(\\(\?:cl-\\)\?def\[cuvm]\\|(setq"
;;; End:
