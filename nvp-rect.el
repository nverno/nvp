;;; nvp-rect.el ---  -*- lexical-binding: t; -*-

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
  ("h" rectangle-backward-char)
  ("l" rectangle-forward-char)
  ("k" rectangle-previous-line)
  ("j" rectangle-next-line)
  ("w" copy-rectangle-as-kill)  ;C-x r M-w
  ("K" kill-rectangle)          ;C-x r k
  ("d" delete-rectangle)        ;C-x r d
  ("c" clear-rectangle)         ;C-x r c
  ("y" yank-rectangle)          ;C-x r y
  ("t" string-rectangle)        ;C-x r t
  ("o" open-rectangle)          ;C-x r o
  ("N" rectangle-number-lines)  ;C-x r N
  ("e" nvp-rect-ex-point-mark)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)))
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
