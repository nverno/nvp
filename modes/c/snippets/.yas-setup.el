(eval-when-compile (require 'nvp-yas))

(defun nvp-header-file-p ()
  (string-match-p "h[xp]*" (nvp-yas-ext)))
