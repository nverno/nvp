;;; nvp-erlang.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(declare-function expand-abbrev-hook "expand")
(declare-function asdf-use "asdf")
(declare-function erlang-mark-clause "erlang")
(declare-function erlang-mark-function "erlang")
(declare-function erlang-compile "erlang")

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
