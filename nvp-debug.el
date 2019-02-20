;;; nvp-debug.el --- Debugging -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Last modified: <2019-02-20 15:18:38>
;; Package-Requires: 
;; Created: 25 November 2016

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
  (nvp-local-vars)
  (defvar tramp-debug-on-error)
  (defvar tramp-verbose)
  (require 'hydra))
(defvar nvp/etc)

;;;###autoload(autoload 'nvp-debug-hydra-emacs/body "nvp-debug")
(nvp-hydra-set-property 'nvp-debug-hydra-emacs)
(defhydra nvp-debug-hydra-emacs (:color blue)
  "
Toggle debugging functions:

^emacs^                       ^edebug^               ^tramp/other^
^^^^^^^^------------------------------------------------------------
toggle on _e_rror              _a_: all defuns         _t_ramp
toggle on _q_uit               _l_: all local defs     _i_: ielm
_n_ew emacs (in debug)         _f_: all forms          _SPC_: cancel
_b_are emacs (-l bare-site)    _c_urrent defun         _d_ebugger
toggle on entr_y_
"
  ("a" edebug-all-defs)
  ("b" nvp-debug-launch-bare-emacs)
  ("c" edebug-defun :color blue)
  ("d" debug :color blue)
  ("e" toggle-debug-on-error)
  ("f" edebug-all-forms)
  ("i" ielm :color blue)
  ("l" nvp-debug-ede-all-local)
  ("n" nvp-debug-launch-new-debug)
  ("q" toggle-debug-on-quit)
  ("SPC" nil)
  ("t" nvp-debug-tramp-toggle-debug)
  ("y" debug-on-entry :color blue))

(defvar nvp-ede-local-active nil)

(defun nvp-debug-ede-all-local ()
  "Toggle debugging of all defuns."
  (interactive)
  (make-local-variable 'edebug-all-defs)
  (edebug-all-defs)
  (setq nvp-ede-local-active (not nvp-ede-local-active))
  (message "Local edebug-all-defs is %s." (if nvp-ede-local-active "on" "off")))

(defun nvp-debug-tramp-toggle-debug ()
  "Toggle `tramp-debug-on-error' on/off."
  (interactive)
  (ignore-errors
    (when (and (file-remote-p (buffer-file-name)))
      (let ((action (and (not (null tramp-debug-on-error))
                         tramp-debug-on-error)))
        (setq tramp-debug-on-error (not action))
        (setq tramp-verbose (if action 0 10))
        (message "set `tramp-debug-on-error' %s" (if action "off" "on"))))))

;;; Launching new emacs instances
(eval-when-compile
  (defmacro nvp-debug--launch-emacs (&rest args)
    (declare (debug defun))
    (let ((args (cons (nvp-with-gnu/w32 "emacs" "runemacs.exe") args)))
      `(call-process-shell-command
        (mapconcat #'shell-quote-argument (delq nil (list ,@args)) " ") nil 0 nil))))

(defun nvp-debug-launch-bare-emacs (&optional arg)
  "Start new emacs instance loading init file that only adds load paths.
With prefix open current file."
  (interactive "P")
  (nvp-debug--launch-emacs
   "-Q" "-l" (expand-file-name "bare-site.el" nvp/etc) (if arg (buffer-file-name))))

(defun nvp-debug-launch-new-debug ()
  "Start new emacs in debug mode."
  (interactive)
  (nvp-debug--launch-emacs "--debug-init"))

(provide 'nvp-debug)
;;; nvp-debug.el ends here
