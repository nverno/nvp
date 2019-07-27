;;; company-d --- 

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/d-tools
;; Package-Requires: 
;; Created:  3 December 2016

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

;; [![Build Status](https://travis-ci.org/nverno/d-tools.svg?branch=master)](https://travis-ci.org/nverno/d-tools)

;; Port from [ac-dcd](https://github.com/atilaneves/ac-dcd) to company.

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'rx))
(require 'company)
(autoload 'd-tools-ensure-process-env "d-tools")

;; dcd-server binary
(defvar company-d-dcd-server "dcd-server")
(defvar company-d-dcd-client "dcd-client")

;; dcd-server config
 ;; '("-I.")
(defvar company-d-dcd-flags nil)
(defvar company-d-dcd-port 9166)

;; regex to parse dcd output
(defvar company-d-dcd-completion-pattern
  (rx bol (submatch (1+ nonl)) "\t"
      (submatch (any "cisuvmkfgePMaAltT")) eol))

(defvar company-d-output-buffer "*dcd-output*")

;; -------------------------------------------------------------------
;;; Server 

;; start dcd-server if not already running
(defsubst company-d-start-server ()
  (when (not (get-process "dcd-server"))
    ;; add binaries to exec path / install if req.
    (d-tools-ensure-process-env)
    (let ((buff (get-buffer-create "*dcd-server*")))
      (with-current-buffer buff
        (apply 'start-process "dcd-server" (current-buffer)
               company-d-dcd-server "-p"
               (format "%s" company-d-dcd-port)
               company-d-dcd-flags)))))

(defun company-d-stop-server ()
  "Stop dcd-server."
  (interactive)
  (interrupt-process "dcd-server"))

;; call dcd-client with ARGS
(defun company-d-execute (args)
  (let ((buff (get-buffer-create company-d-output-buffer)))
    ;; (with-current-buffer buff (erase-buffer))
    (apply 'call-process-region (point-min) (point-max)
           company-d-dcd-client nil buff nil
           (cons "--tcp" args))))

(defsubst company-d--cursor ()
  (position-bytes (point)))

(defsubst company-d--args (pos)
  (list "-c" (format "%s" pos) "-p" (format "%s" company-d-dcd-port)))

;; get completion candidates from server
(defun company-d-get-candidates ()
  (unless (nth 8 (syntax-ppss))
    (company-d-execute (company-d--args (company-d--cursor)))))

(provide 'company-d)
;;; company-d.el ends here
