;;; ocaml-help.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/ocaml-tools
;; Package-Requires: 
;; Created:  8 December 2016

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
  (require 'ocaml-tools)
  (require 'cl-lib))
(require 'ocaml-tools)
(require 'merlin-company)
(autoload 'ocaml-module-alist "caml-help")

;; #<marker at 103230 in tuareg.el>
;; #<marker at 5437 in caml-help.el>
;; caml-help:  ocaml-module-alist, ocaml-visible-modules,
;;             ocaml-module-symbols, ocaml-open-module
;; #<marker at 99381 in tuareg.el>
;; names: tuareg-current-fun-name

(defvar ocaml-help-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?. "_" st)
    st))

;; -------------------------------------------------------------------
;;; Utils

(eval-when-compile
  (defmacro with-url-buffer (url &rest body)
   (declare (indent defun) (debug defun))
   `(with-current-buffer (url-retrieve-synchronously ,url)
      ,@body
      (kill-buffer))))

;; -------------------------------------------------------------------
;;; Help at point

;; display help for function at point in popup tooltip
;;;###autoload
(defun ocaml-help-at-point ()
  (interactive)
  (with-syntax-table ocaml-help-syntax-table
    (let ((candidate (thing-at-point 'symbol)))
      (when candidate
        (let ((info (car-safe (merlin/complete candidate))))
          (when info
            (let ((doc (cdr (assoc 'info info)))
                  (type (cdr (assoc 'desc info))))
              (nvp-with-toggled-tip
                (concat "val " candidate " : " type "\n\n(** " doc " *)")))))))))

;;;###autoload
(defun ocaml-help-zeal-at-point (arg)
  (interactive "P")
  (with-syntax-table ocaml-help-syntax-table
    (funcall-interactively 'nvp-help-zeal-at-point arg)))

;; -------------------------------------------------------------------
;;; Library
;; TODO:
;; - better version of `tuareg-browse-library'
;;   should be smart about library path, and offer completing read
;; (ocaml-tools-read "Module: " :module)

;; +-----------------------------------------------------------------+
;; |                           Online                                |
;; +-----------------------------------------------------------------+
;; TODO:
;; - better version of `tuareg-browse-manual'

(defvar ocaml-help-topics
  '((".merlin" .
     "https://github.com/the-lambda-church/merlin/wiki/project-configuration")
    ("cheatsheets" . 'ocaml-help-cheatsheets)
    ("project" . "https://github.com/kmicinski/example-ocaml-merlin")
    ("performance" .
     "https://janestreet.github.io/ocaml-perf-notes.html")))

(defvar ocaml-help-cheatsheets
  '("ocaml-lang" "ocaml-tools" "ocaml-stdlib" "tuareg-mode" "all"))

;; read index of modules in online manual
(defvar ocaml-help-manual-modules ())
(defun ocaml-help-manual-modules ()
  (or ocaml-help-manual-modules
      (let (res)
        (with-url-buffer
          "https://caml.inria.fr/pub/docs/manual-ocaml/libref/index_modules.html"
          (goto-char (point-min))
          (while (not (looking-at-p "</head>"))
            (when (looking-at ".*href=\"\\([[:alnum:]]+\\)\.html\"")
              (push (match-string 1) res))
            (forward-line)))
        (setq ocaml-help-manual-modules (nreverse res)))))

;; lookup help for module in online manual with completing read
;;;###autoload
(defun ocaml-help-browse-manual-online (module)
  (interactive
   (list (ocaml-tools-read "Module: " (ocaml-help-manual-modules))))
  (browse-url
   (concat "https://caml.inria.fr/pub/docs/manual-ocaml/libref/"
           module ".html")))

;;;###autoload
(defun ocaml-help-online (topic)
  (interactive
   (list (cdr (assoc (ocaml-tools-read "Topic: " 'ocaml-help-topics)
                     ocaml-help-topics))))
  (pcase topic
    (`(quote ,sym)
     (call-interactively sym))
    ((pred stringp)
     (browse-url topic))))

(defun ocaml-help-cheatsheets (sheet)
  (interactive
   (list (ocaml-tools-read "Cheatsheet: " ocaml-help-cheatsheets)))
  (browse-url
   (if (string= "all" sheet)
       "https://www.typerex.org/cheatsheets.html"
     (format "https://www.typerex.org/files/cheatsheets/%s.pdf"
             sheet))))

(provide 'ocaml-help)
;;; ocaml-help.el ends here
