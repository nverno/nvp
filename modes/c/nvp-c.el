;;; nvp-c.el --- ... -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/c-tools
;; Package-Requires: 
;; Created: 11 November 2016

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

;; [![Build Status](https://travis-ci.org/nverno/c-tools.svg?branch=master)](https://travis-ci.org/nverno/c-tools)

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro)
  (defvar c/R-abbrev-table)
  (defvar nvp-abbrev-local-table))
(declare-function xref-push-marker-stack "xref")
(nvp-declare "cc-cmds" c-mark-function c-beginning-of-defun)
(nvp-declare "" nvp-log nvp-compile nvp-compile-cmake)
(autoload 'string-trim-right "subr-x")

;;; TODO:
;; - Move env setup ffi - R, lisp - to separate file.
;;   Should do env., snippets, abbrevs, etc all together.
;;; FIXME:
;; - function signatures => generic parsing method

(nvp-package-define-root :snippets t)

(defvar-local nvp-c-local-include-paths nil)
(setq-default nvp-c-local-include-paths '("." ".." "../include"))

;; -------------------------------------------------------------------
;;; Util

(defsubst nvp-c-out-file (&optional file)
  (concat (file-name-sans-extension (or file (buffer-file-name)))
          (nvp-with-gnu/w32 ".out" ".exe")))

;; associated header file name
(defsubst nvp-c--header-file-name (&optional buffer)
  (concat (file-name-sans-extension (or buffer buffer-file-name)) ".h"))

;; split string STR on commas, but only when not between <..>
;; eg., "std::vector<std::pair<int,int>> i, int j" =>
;;      ("std::vector<std::pair<int,int>> i" "int j")
(defun nvp-c-split-string (str &optional delim)
  (when (not (zerop (length str)))
    (let ((delim (or delim ?\,))
          (bcount 0)                     ; current opening brace count
          (prev 0)                       ; substring starting location
          (trim-p t)                     ; non-nil if skipping beginning blanks
          res)                           ; list of resulting strings
      (cl-loop for c across str
         for i from 0 upto (length str)
         do (pcase c
              (`?  (and trim-p (cl-incf prev)))
              (`?< (cl-incf bcount))
              (`?> (cl-decf bcount))
              ((pred (equal delim))
               (when (zerop bcount)
                 (push (substring str prev i) res)
                 (setf prev (1+ i))
                 (setf trim-p t)))
              (_ (setf trim-p nil))))
      (push (string-trim-right (substring str prev)) res)
      (nreverse res))))

;; pull out functions signatures from current buffer using ctags
(defun nvp-c-function-signatures (&optional file ignore-main ignore-static)
  (when-let* ((sigs (process-lines
               (nvp-program "ctags" 'no-compile) "-x" "--c-kinds=fp"
               (or file buffer-file-name)))
              
              (res (mapcar
                    (lambda (s)
                      (string-trim-left
                       (replace-regexp-in-string
                        "[ \t;{]*$" ""
                        (cadr (split-string s (or file buffer-file-name) t " ")))))
                    (if ignore-main
                        (cl-remove-if #'(lambda (s) (string-prefix-p "main" s)) sigs)
                      sigs))))
    (if ignore-static
        (cl-remove-if #'(lambda (s) (string-match-p "\\_<static\\_>" s)) res)
      res)))

;; -------------------------------------------------------------------
;;; Environment

(declare-function asdf-where "asdf")
(declare-function nvp-env-add "nvp-env")

(defvar nvp-c-ext-includes
  '(("unity" (expand-file-name ".local/include/unity/src" (getenv "HOME"))
     "/unity/src")
    ("R"     (expand-file-name "lib/R/include" (asdf-where "R")) "/R/include")
    ("emacs" emacs-src-dir "/emacs/src"))
  "Paths to external includes.")

;; set environment stuff for macro expanding
;; could also set local `c-macro-preprocessor'?
(defun nvp-c-setenv (type)
  "Add include path of TYPE to macroexpand all the shittles."
  (interactive
   (list (ido-completing-read "Add include path for: " nvp-c-ext-includes)))
  (cl-destructuring-bind (kind loc regex) (assoc-string type nvp-c-ext-includes)
    (pcase kind
      (`"R"
       (setq-local local-abbrev-table c/R-abbrev-table)
       (setq-local nvp-abbrev-local-table "c/R"))
      (_ nil))
    (nvp-env-add "C_INCLUDE_PATH" (eval loc) regex)))

;; ------------------------------------------------------------
;;; Commands

(nvp-newline nvp-c-newline-dwim nil
  :pairs (("{" "}"))
  :comment-re (" *\\(?:/\\*\\|\\*\\)" . "\\*/ *")
  :comment-start "* ")

(defun nvp-c-newline-x ()
  (interactive)
  (end-of-line)
  (delete-horizontal-space)
  (unless (eq (char-before) ?\;)
    (insert ";"))
  (newline-and-indent))

;;;-- Marking --

(defun nvp-c-mark-defun ()
  (interactive)
  (nvp--mark-defun                      ;fixme
   ;; mark function on first invoke
   (c-mark-function)
   ;; successively extend to next functions
   (c-beginning-of-defun -1)
   (point)))

;; -------------------------------------------------------------------
;;; Compile

;; run make / cmake if there are corresponding makefiles,
;; otherwise prompt / use default
(nvp-make-or-compile-fn nvp-c-compile
  (:default-prompt (read-from-minibuffer "Compiler flags: "))
  (let* ((flags (or args "-Wall -Werror -O2 -g -std=c11"))
         (file (file-name-nondirectory buffer-file-name))
         (out (file-name-sans-extension file))
         (compile-command
          (format "%s %s -o %s%s %s" (nvp-program "gcc")
                  flags out (nvp-with-gnu/w32 ".out" ".exe") file)))
    (setq-local compile-command compile-command)
    (call-interactively 'nvp-compile)))

;; compile current file and run it with output to compilation buffer
(defun nvp-c-compile-and-run (keep &optional compiler flags post-action)
  (interactive "P")
  (let* ((out (concat (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))
                      (nvp-with-gnu/w32 ".out" ".exe")))
         (command
          (concat (or compiler (nvp-program "gcc")) " "
                  (or flags "-s -O3") " "
                  buffer-file-name " -o " out "; "
                  (or (and (eq post-action 'no-run) "")
                      post-action (concat "./" out))
                  (unless keep (concat "; rm " out)))))
    (setq-local compile-command command)
    (funcall-interactively 'nvp-compile nil current-prefix-arg)))

;; watch error output with TEST
(defun nvp-c-compile-watch (arg)
  (interactive "P")
  (let ((file (if arg (read-from-minibuffer "Output file: " "out.txt")
                "out.txt"))
        (out (file-name-nondirectory (nvp-c-out-file))))
    (nvp-c-compile-and-run
     nil nil "-O3 -DTEST -std=c11"
     (concat "./" out " 2> " file
             "& gnome-terminal -x watch tail -n10 " file))))

(defun nvp-c-compile-debug ()
  (interactive)
  (nvp-c-compile-and-run 'keep nil "-Wall -Werror -ggdb3 -DDEBUG" 'no-run)
  (call-interactively 'gdb))

;; show assembly in other window, delete assembly output
(defun nvp-c-compile-asm ()
  (interactive)
  (let ((compile-command (format "gcc -Og -S %s" buffer-file-name))
        (asm-file
         (concat (file-name-sans-extension buffer-file-name) ".s")))
    (with-current-buffer (call-interactively 'nvp-compile)
      (pop-to-buffer (current-buffer))
      (add-hook 'compilation-finish-functions
                (lambda (_b _s)
                  (find-file-other-window asm-file)
                  (add-hook 'kill-buffer-hook
                            (lambda () (delete-file buffer-file-name))
                            nil 'local))
                nil 'local))))

;; dump objects in compilation buffer, setup imenu for function jumps
;; FIXME: add tab/backtab movement
;; (autoload 'gdb-disassembly-mode "gdb-mi")
(declare-function objdump-mode "objdump")
(defun nvp-c-compile-objdump ()
  (interactive)
  (let ((compile-command (format "gcc -Og -c %s; objdump -d %s.o; rm %s.o"
                                 buffer-file-name
                                 (file-name-sans-extension buffer-file-name)
                                 (file-name-sans-extension buffer-file-name)))
        compilation-scroll-output)
    (with-current-buffer (call-interactively 'nvp-compile)
      (pop-to-buffer (current-buffer))
      ;; (gdb-disassembly-mode)
      (objdump-mode)
      (setq-local imenu-generic-expression '((nil "^[0-9]+ <\\([^>]+\\)>:" 1)))
      (add-hook 'compilation-finish-functions
                (lambda (_b _s)
                  (search-forward "Disassembly" nil 'move 1))
                nil 'local))))

(defun nvp-c-compile-strace (&optional arg)
  (interactive "P")
  (let* ((prog (file-name-sans-extension buffer-file-name))
         (strace-file (concat prog ".strace"))
         (compile-command (format (nvp-concat "gcc -Og %s -o %s; "
                                              "strace -o %s %s %s")
                                  buffer-file-name
                                  prog
                                  strace-file
                                  prog
                                  (if arg
                                      (read-from-minibuffer
                                       "Additional arguments to function: ")
                                    "")))
         compilation-scroll-output)
    (with-current-buffer (call-interactively 'nvp-compile)
      (pop-to-buffer (current-buffer))
      (add-hook 'compilation-finish-functions
                (lambda (_b _s)
                  (find-file-other-window strace-file)
                  (add-hook 'kill-buffer-hook
                            (lambda () (delete-file buffer-file-name))
                            nil 'local))
                nil 'local))))
  
;; -------------------------------------------------------------------
;;; Headers

(eval-when-compile
  (defvar yas-selected-text)
  (defvar yas-wrap-around-region))
(declare-function yas-expand-snippet "yasnippet")
(declare-function yas-lookup-snippet "yasnippet")

;; jump to associated header, with arg create and/or update it as well
(defun nvp-c-jump-or-update-header (update)
  (interactive "P")
  (if update
      (call-interactively 'nvp-c-create-or-update-header)
    (condition-case nil
        
        (find-file-other-window (nvp-c--header-file-name)))))

;;; Create/update header file with function signatures
(defun nvp-c-create-or-update-header (and-go)
  (interactive (list t))
  (let ((header (nvp-c--header-file-name))
        (sigs (nvp-c-function-signatures nil 'ignore-main 'ignore-static))
        (yas-wrap-around-region nil)
        (init t))
    (when (file-exists-p header)
      (setq init nil)
      (setq sigs
            ;; remove any signatures that are already found in the header file
            (cl-set-difference
             sigs
             (nvp-c-function-signatures header) :test 'string=)))
    (when (or init sigs)
      (with-current-buffer (find-file header)
        (and sigs
             (setq sigs (concat "\n" (mapconcat 'identity sigs ";\n") ";\n")))
        (if init
            ;; let ((yas-selected-text sigs))
            (yas-expand-snippet
             (yas-lookup-snippet "header" 'cc-mode)
             nil nil `((function-signatures ,sigs)))
          ;; insert at end, before final #endif
          (goto-char (point-max))
          (skip-chars-backward " \t\n\r\v") ;skip any trailing whitespace
          (forward-line -1)
          (insert sigs))))
    (when and-go
      (xref-push-marker-stack)
      (find-file header))))

;; add header guard
(defun nvp-c-add-guard ()
  (interactive)
  (let ((guard (concat
                (upcase (file-name-sans-extension
                         (file-name-nondirectory buffer-file-name)))
                "_H")))
    (save-excursion
      (goto-char (point-min))
      (unless (looking-at-p (format "#ifndef _%s" guard))
        (insert (format "#ifndef _%s\n#define _%s\n\n" guard guard))
        (goto-char (point-max))
        (insert (format "\n#endif /* _%s */" guard))))))

;; -------------------------------------------------------------------
;;; Doxygen

(defun nvp-c-toggle-doxygen ()
  (interactive)
  (save-excursion
    (when (re-search-forward "\\(?://\\|/\\*+\\)" nil 'move)
      (if (and (string= (match-string 0) "/**") (eq (char-after) ?<))
          (progn (delete-char -1)
                 (delete-char 1))
        (delete-char -1)
        (insert "**<")
        (end-of-line)
        (unless (looking-back "\\*/\\s-*" (line-beginning-position))
          (delete-horizontal-space)
          (insert " */"))))))

;; align comment start / end for doxygen region
(eval-when-compile
  (defvar align-to-tab-stop))
(defun nvp-c-align-doxygen (beg end)
  (interactive "*r")
  (let (indent-tabs-mode align-to-tab-stop)
    (align-regexp beg end "\\(\\s-*\\)/\\*\\*")
    (align-regexp beg end "\\(\\s-*\\)\\*/")))

;; -------------------------------------------------------------------
;;; Snippets / Yas

(eval-when-compile
  (defvar yas-text))

;; get variable name from declaration, either with type or not
;; eg., i = 1 or int i = 1 => `i'
(defun c-yas-var (text)
  (if (< (length text) 1)
      ""
   (let* ((str (car (split-string text "=" t " ")))
          (strs (split-string str nil t " ")))
     (or (cadr strs) (car strs)))))

;; convert functions args to doxygen params
(defun c-yas-args-docstring (text)
  (let ((args (nvp-c-split-string text)))
    (and args
         (mapconcat 'identity
                    (mapcar (lambda (s) (concat "\n * @param " s)) args) ""))))

;; -------------------------------------------------------------------
;;; Abbrevs

(defun nvp-c-abbrev-expand-p ()
  (not (memq last-input-event '(?_))))

(provide 'nvp-c)
;;; nvp-c.el ends here
