(eval-when-compile
  (defvar yas-text))

(add-hook 'go-mode-hook
          #'(lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))

(declare-function yas-define-snippets "yasnippet")
