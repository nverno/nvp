;;; nvp-erlang.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
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

(defun nvp-erlang-forward-defun ()
  (interactive)
  (forward-sentence 2)
  (backward-sentence))

(defun nvp-erlang-mark-dwim ()
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
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-erlang.el ends here
