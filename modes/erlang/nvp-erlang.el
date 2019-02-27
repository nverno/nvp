;;; nvp-erlang.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-15 10:38:41>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/erlang-tools
;; Package-Requires: 
;; Created: 20 December 2016

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

;; [![Build Status](https://travis-ci.org/nverno/erlang-tools.svg?branch=master)](https://travis-ci.org/nverno/erlang-tools)

;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(declare-function expand-abbrev-hook "expand")
(declare-function asdf-use "asdf")
(declare-function erlang-mark-clause "erlang")
(declare-function erlang-mark-function "erlang")
(declare-function erlang-compile "erlang")

(nvp-package-define-root :snippets t)

;; -------------------------------------------------------------------
;;; Install

;; (defun nvp-erlang-install ()
;;   (interactive)
;;   (nvp-with-install-script nvp-erlang--dir "install_erl_deps" 'sudo
;;     (nvp-with-install-script nvp-erlang--dir "install")))

;; -------------------------------------------------------------------
;;; Tag

;; Tag all the erlang files in current directory and subdirs.
(defun nvp-erlang-etag ()
  (interactive)
  (call-process-shell-command
   "find . -name \"*.[he]rl\" -print | etags - " nil nil nil))

;; -------------------------------------------------------------------
;;; Commands

(defsubst nvp-erlang-forward-defun ()
  (interactive)
  (forward-sentence 2)
  (backward-sentence))

(defsubst nvp-erlang-mark-dwim ()
  (interactive)
  (if (not (eq last-command this-command))
      (erlang-mark-clause)
    (call-interactively 'erlang-mark-function)))

;;; compile

(defun nvp-erlang-compile ()
  (interactive)
  (when (equal current-prefix-arg '(16))
    (call-interactively 'asdf-use))
  (call-interactively 'erlang-compile))

(provide 'nvp-erlang)
;;; nvp-erlang.el ends here
