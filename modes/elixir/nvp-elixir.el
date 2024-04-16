;;; nvp-elixir.el --- Elixir -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(nvp:decls :p (elixir alchemist))


;;; Repl

(autoload 'alchemist-iex-process "alchemist-iex")

(with-eval-after-load 'nvp-repl
  ;; TODO: start repl with mix: `alchemist-iex-project-run'
  (nvp-repl-add '(elixir-mode elixir-ts-mode)
    :name 'iex
    :modes '(alchemist-iex-mode)
    :init #'alchemist-iex-process
    :find-fn (lambda () (ignore-errors (get-buffer alchemist-iex-buffer)))
    :send-string #'alchemist-iex--send-command
    ;; :send-sexp #'alchemist-iex-send-last-sexp
    ;; TODO:
    ;; - `alchemist-iex-compile-this-buffer'
    ;; - `alchemist-iex-reload-module'
    ;; Eval: `alchemist-eval'
    :cd-cmd "cd(\"%s\")"
    :pwd-cmd "pwd"
    :help-cmd '(:no-arg "h" :with-arg "h(%s)")))

(provide 'nvp-elixir)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-elixir.el ends here
