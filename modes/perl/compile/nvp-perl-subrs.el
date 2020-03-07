;;; nvp-perl-subrs.el --- randoms -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp-decls)

;; Skip back across `backchars' chars, then look for `forward-regexp',
;; returning cons of start and end of match.
(defsubst nvp-back-chars-then-look (backchars &optional forward-regexp)
  (or forward-regexp (setq forward-regexp (format "[%s]+" backchars)))
  (save-excursion
    (skip-chars-backward backchars)
    (if (looking-at forward-regexp)
        (cons (point) (match-end 0))
      nil)))

(defsubst nvp-perl-replace-all (from to str)
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string from to str t t))

(provide 'nvp-perl-subrs)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-perl-subrs.el ends here
