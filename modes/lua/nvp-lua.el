;;; nvp-lua.el --- lua extensions -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'lua-ts-mode nil t)

(nvp:decls :p (lua))

;;; REPL
(with-eval-after-load 'nvp-repl
  (require 'nvp-lua-repl))

;;; Fold
(defvar hs-special-modes-alist)
(with-eval-after-load 'hideshow
  (let ((rules '("\\(?:local \\)?function\\|if\\|do\\|while\\|for\\|{\\|\\[\\["
                 "end\\|}\\|\\]\\]" "--" nil)))
    (dolist (mode '(lua-ts-mode lua-mode))
      (if (assq mode hs-special-modes-alist)
          (setf (cdr (assq 'lua-ts-mode hs-special-modes-alist)) rules)
        (push `(,mode ,@rules) hs-special-modes-alist)))))

;;; Snippets
(nvp:decl nvp-yas-split-args yas-field-value)

;; return keys from 'key1 [= val1][, key_i [= val_i]]*'
(defun nvp-lua--keys (&optional str)
  (nvp-yas-split-args (or str yas-text) "[ \t]*=[^,]*,?[ \t]*"))

;; produce "-" when range is decreasing from INIT to LIMIT
(defun nvp-lua--step-sign (init limit)
  (let ((vinit (yas-field-value init))
        (vlim (yas-field-value limit)))
    (when (and vinit vlim (> (string-to-number vinit) (string-to-number vlim)))
      "-")))

(provide 'nvp-lua)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-lua.el ends here
