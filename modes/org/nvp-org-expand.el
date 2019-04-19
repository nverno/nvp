;;; nvp-org-expand.el ---  -*- lexical-binding: t; -*-

;; Last modified: <2019-04-19.13>
;; URL: https://github.com/nverno/md-tools
;; Created:  2 December 2016

;;; Commentary:
;; from wiki
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'org)
  (require 'hydra))
(declare-function org-try-structure-completion "org")

;; Expands "<" at beginning of line
(nvp-hydra-set-property 'nvp-org-src-hydra)
(defhydra nvp-org-src-hydra (:color blue)
  "
_c_enter  _q_uote    _L_aTeX:
_l_atex   _e_xample  _i_ndex:
_a_scii   _v_erse    _I_NCLUDE:
_s_rc     ^ ^        _H_TML:
_h_tml    ^ ^        _A_SCII:
"
  ("s" (nvp-org-src-expand "<s"))
  ("e" (nvp-org-src-expand "<e"))
  ("q" (nvp-org-src-expand "<q"))
  ("v" (nvp-org-src-expand "<v"))
  ("c" (nvp-org-src-expand "<c"))
  ("l" (nvp-org-src-expand "<l"))
  ("h" (nvp-org-src-expand "<h"))
  ("a" (nvp-org-src-expand "<a"))
  ("L" (nvp-org-src-expand "<L"))
  ("i" (nvp-org-src-expand "<i"))
  ("I" (nvp-org-src-expand "<I"))
  ("H" (nvp-org-src-expand "<H"))
  ("A" (nvp-org-src-expand "<A"))
  ("t" (nvp-org-src-expand "<t"))
  ("<" self-insert-command "ins")
  ("o" nil "quit"))

;; Expand org template
(defun nvp-org-src-expand (str)
  (insert str)
  (org-try-structure-completion))

;;;###autoload
(defun nvp-org-src-maybe ()
  (interactive)
  (if (looking-back "^" (line-beginning-position))
      (nvp-org-src-hydra/body)
    (self-insert-command 1)))

(provide 'nvp-org-expand)
;;; nvp-org-expand.el ends here
