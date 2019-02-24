;;; nvp-smerge.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-24 04:51:05>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 31 January 2019

;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro)
  (require 'hydra))
(require 'smerge-mode)

(nvp-hydra-set-property 'nvp-hydra-smerge :verbosity 1)
;;;###autoload(autoload 'nvp-hydra-smerge/body "nvp-smerge")
(defhydra nvp-hydra-smerge
  (:color red :hint nil :pre (smerge-mode 1))
  "
^Move^ ^Keep^ ^Diff^ ^Pair^
------------------------------------------------------
_n_ext _b_ase  _R_efine  _<_: base-mine
_p_rev _m_ine  _E_diff   _=_: mine-other
^ ^    _o_ther _C_ombine _>_: base-other
^ ^    _a_ll   _r_esolve
_q_uit _RET_: current
"
  ("RET" smerge-keep-current)
  ("C" smerge-combine-with-next)
  ("E" smerge-ediff)
  ("R" smerge-refine)
  ("a" smerge-keep-all)
  ("b" smerge-keep-base)
  ("m" smerge-keep-upper)
  ("n" smerge-next)
  ("o" smerge-keep-lower)
  ("p" smerge-prev)
  ("r" smerge-resolve)
  ("<" smerge-diff-base-upper)
  ("=" smerge-diff-upper-lower)
  (">" smerge-diff-base-lower)
  ("q" nil :color blue))

(provide 'nvp-smerge)
;;; nvp-smerge.el ends here
