;;; nvp-rect.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-02-24 04:52:48>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 20 December 2016

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'hydra))
(require 'rect)

;;;###autoload(autoload 'nvp-rect-hydra/body "nvp-rect")
(nvp-hydra-set-property 'nvp-rect-hydra)
(defhydra nvp-rect-hydra (:body-pre (rectangle-mark-mode 1)
                          :color pink
                          :post (deactivate-mark))
  "
  ^_k_^     _d_elete    _t_ype    _N_umber-lines
_h_   _l_   _y_ank      _o_pen    _e_xchange-point
  ^_j_^     _w_ copy    _r_eset   _r_eset-region-mark
^^^^        _K_ill      _c_lear   _g_ quit
^^^^        _u_ndo      ^ ^        ^ ^
"
  ("h" rectangle-backward-char nil)
  ("l" rectangle-forward-char nil)
  ("k" rectangle-previous-line nil)
  ("j" rectangle-next-line nil)
  ("w" copy-rectangle-as-kill nil)      ;C-x r M-w
  ("K" kill-rectangle)                  ;C-x r k
  ("d" delete-rectangle nil)            ;C-x r d
  ("c" clear-rectangle)                 ;C-x r c
  ("y" yank-rectangle nil)              ;C-x r y
  ("t" string-rectangle nil)            ;C-x r t
  ("o" open-rectangle nil)              ;C-x r o
  ("N" rectangle-number-lines)          ;C-x r N
  ("e" nvp-rect-ex-point-mark nil)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("u" undo nil)
  ("g" nil))

;; Exchange point and mark.
(defun nvp-rect-ex-point-mark ()
  (interactive)
  (if rectangle-mark-mode
      (rectangle-exchange-point-and-mark)
    (let ((mk (mark)))
      (rectangle-mark-mode 1)
      (goto-char mk))))

(provide 'nvp-rect)
;;; nvp-rect.el ends here
