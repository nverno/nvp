;;; nvp-browse ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Package-Requires: 
;; Created:  2 December 2016

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
  (require 'cl-lib)
  (defvar webjump-sites))

;; local uris to jump to
(defvar nvp-local-uris nil)

;;;###autoload
(defun nvp-browse-start ()
  (interactive)
  (call-process (nvp-program "firefox") nil 0 nil))

;;;###autoload
(defun nvp-browse-url-contents ()
  "Open a new buffer containing the contents of a URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
	 (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    (cond ((search-forward "<?xml" nil t) (xml-mode))
	  ((search-forward "<html" nil t) (web-mode)))))

;;--- Webjump --------------------------------------------------------

(defvar nvp-webjump-sites
  '(
    ("Gmail" . "www.gmail.com")
    ("Bitbucket" .
     [simple-query "www.bitbucket.org/nverno"
                   "www.bitbucket.org/nverno/" ""])
    ("Github" .
     [simple-query "www.github.com"
                   "www.github.com/" ""])
    ("Bookfi".
     [simple-query "en.bookfi.net"
                   "en.bookfi.net/s/?q=" "&t=0"])
    ("Stackoverflow" .
     [simple-query "www.stackoverflow.com"
                   "www.stackoverflow.com/search?q=" ""])
    ("Dzone" . "www.dzone.com/search")
    ("Unblocked" . "unblocked-pw.github.io/")
    ("Ubuntu" .
     [simple-query "packages.ubuntu.com"
                   "packages.ubuntu.com/search?keywords=" ""])
    ("Amazon" .
     [simple-query "www.amazon.com"
                   "www.amazon.com/s/ref=nb_sb_noss_2?url=search-alias%3Daps&field-keywords=" ""])
    ("Stanford". "www.lagunita.stanford.edu/courses")
    ("Edx" .
     [simple-query "www.edx.org"
                   "www.edx.org/course?search_query=" ""])
    ("Coursera" .
     [simple-query "www.coursera.org"
                   "www.coursera.org/courses?languages=en&query=" ""])
    ("Torrents" . "https://unblocked-pw.github.io/")
    ("Pirate Bay" . "https://proxybay.one/")
    ("Kickass" .
     [simple-query "https://kickass.unblocked.live"
                   "https://kickass.unblocked.live/search.php?q=" ""])
    ("Choco" .
     [simple-query "https://chocolatey.org/packages"
                   "https://chocolatey.org/packages?q=" ""])
    ("Omniref" .
     [simple-query "https://docs.omniref.com/"
                   "https://docs.omniref.com/?q=" ""])
    ("Travis" . "https://travis-ci.org/nverno")))

;; Webjump interface
;;;###autoload
(defun nvp-browse-webjump (&optional arg)
  (interactive "P")
  (require 'webjump)
  (let* ((completion-ignore-case t)
         (locals (or (and arg (read-from-minibuffer "URI: "))
                     (and (bound-and-true-p nvp-local-uris)
                          (y-or-n-p "Use local uris?")
                          nvp-local-uris)))
         (sites (or locals
                    (append nvp-webjump-sites webjump-sites)))
         (item (or (and arg (cons nil locals))
                   (assoc-string
                    (ido-completing-read "WebJump to site: "
                                         (mapcar 'car sites) nil t)
                    sites t)))
         (name (car item))
         (expr (cdr item)))
    (browse-url (webjump-url-fix
                 (cond ((not expr) "")
                       ((stringp expr) expr)
                       ((vectorp expr) (webjump-builtin expr name))
                       ((listp expr) (eval expr))
                       ((symbolp expr)
                        (if (fboundp expr)
                            (funcall expr name)
                          (error "WebJump URL function \"%s\" undefined"
                                 expr)))
                       (t (error "WebJump URL expression for \"%s\" invalid"
                                 name)))))))

;; -------------------------------------------------------------------

(declare-function thing-at-point-url-at-point "thingatpt")
(declare-function webjump-url-fix "webjump")
(declare-function webjump-builtin "webjump")
(declare-function web-mode "web-mode")

(provide 'nvp-browse)
;;; nvp-browse.el ends here
