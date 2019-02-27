;;; nvp-c-debug.el --- C debugging -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/c-debug
;; Last modified: <2019-02-20 13:33:40>
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
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'hydra))
(require 'gud)
(nvp-declare "" nvp-indicate-cursor-pre nvp-indicate-cursor-post
  nvp-he-history-setup nvp-comint-setup-history)

;; -------------------------------------------------------------------
;;; GDB REPL

;;; FIXME: gud-mode seems to clobber kill-buffer-hooks,
;;         so shell history isn't being saved/read properly
;;;###autoload
(defun nvp-gud-repl-setup ()
  (require 'nvp-comint)
  (nvp-he-history-setup :history 'comint-input-ring
                        :bol-fn 'comint-line-beginning-position)
  (nvp-comint-setup-history ".gdb_history"))

;;;###autoload(autoload 'nvp-gud-repl-switch "c-debug")
(nvp-repl-switch "gud" (:repl-mode 'gud-mode
                        :repl-find-fn
                        #'(lambda ()
                            (and (comint-check-proc gud-comint-buffer)
                                 gud-comint-buffer)))
  ;; FIXME: how to get the buffer returned from interactive call
  ;; and add source buffer property after GDB has started?
  (call-interactively 'gdb))

;; -------------------------------------------------------------------
;;; GDB Hydra

(nvp-bindings-multiple-modes (("c"   . cc-mode)
                              ("c++" . cc-mode))
  ("<f2> d g" . c-debug-gud-hydra/body))

;; compiler doesnt understande these functions
(with-no-warnings
  (defhydra c-debug-gud-hydra (:color amaranth
                               :pre nvp-indicate-cursor-pre
                               :post nvp-indicate-cursor-post)
    ;; vi
    ("h" backward-char nil)
    ("j" next-line nil)
    ("k" previous-line nil)
    ("l" forward-char nil)
    ;; gud
    ("m" gud-many-windows "many-windows mode")
    ("t" gud-tbreak "tbreak")
    ("b" gud-break "break")
    ("d" gud-remove "remove")
    ;; ("D" )
    ("J" gud-jump "jump")
    ("p" gud-print "print")
    ("m" gud-until "move")
    ("n" gud-next "next")
    ("c" gud-cont "cont")
    ("o" gud-finish "out")
    ("r" gud-run "run")
    ("q" nil "quit")))
(nvp-hydra-set-property 'c-debug-gud-hydra)

(provide 'nvp-c-debug)
;;; nvp-c-debug.el ends here
