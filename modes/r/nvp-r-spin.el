;;; nvp-r-spin.el --- spin rmarkdown -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/r-tools
;; Last modified: <2019-01-15 18:47:52>
;; Package-Requires: 
;; Created:  5 September 2017

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
  (require 'nvp-macro))

(declare-function ess-send-string "ess-inf")
(declare-function ess-get-words-from-vector "ess-inf")
(declare-function browse-url-file-url "browse-url")

;; spin yarn and show result in browser or open if it is a pdf
;; use ess-process when available, otherwise try rscript
;;;###autoload
(defun nvp-r-spin-file (file)
  (interactive
   (list (or (and (eq major-mode 'ess-r-mode) buffer-file-name)
             (read-file-name "File to spin: "))))
  (let ((proc (and (fboundp 'ess-get-process)
                   (ignore-errors (ess-get-process nil t))))
        (cmd (nvp-concat "local({ "
                         "out <- rmarkdown::render(\"%s\");"
                         "if (tools::file_ext(out) == \"html\") {"
                         "    browseURL(out);"
                         "} else {"
                         "    out;"
                         "}"
                         "})\n")))
    (if proc
        (let ((res (ess-get-words-from-vector (format cmd file))))
          (pcase (file-name-extension (car res))
            (`"pdf" (find-file-other-window (car res)))
            (_ (browse-url-file-url (car res)))))
      ;; FIXME: not tested, doesn't open or return pdf
      (start-process-shell-command
       "rscript" nil
       (concat "Rscript -e \"" (format cmd file) "\"")))))

(provide 'nvp-r-spin)
;;; nvp-r-spin.el ends here
