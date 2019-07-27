(eval-when-compile
  (defvar yas-text)
  (defvar yas-indent-line))
(declare-function yas-define-snippets "yasnippet")

(add-hook 'coffee-mode-hook
          #'(lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))
