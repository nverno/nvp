;;; coffee-repl.el --- Coffee REPL -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'coffee-mode nil t)
(require 'comint)
(nvp:decls :p (coffee))

(setq-default coffee-repl-buffer "*coffee*")

(defcustom coffee-repl-prompt "coffee> "
  "Regexp matching coffee repl prompt."
  :group 'coffee
  :type 'regexp
  :safe 'stringp)

(defun coffee-repl--preoutput-filter (string)
  ;; Ignore repeated prompts when switching windows
  (if (and (not (bolp))
           (string-match-p
            (rx-to-string
             `(seq bos (+ (regexp ,coffee-repl-prompt)) eos))
            string))
      ""
    ;; Filter out the extra prompt characters that
    ;; accumulate in the output when sending regions
    ;; to the inferior process.
    (replace-regexp-in-string
     (rx-to-string `(: bol
                       (* (regexp ,coffee-repl-prompt))
                       (group (regexp ,coffee-repl-prompt) (* nonl))))
     "\\1" string)))

(defun coffee-repl-buffer ()
  "Return inferior Coffee buffer if there is an active one."
  (if (derived-mode-p 'coffee-repl-mode)
      (current-buffer)
    (when (comint-check-proc coffee-repl-buffer)
      coffee-repl-buffer)))

(defun coffee-repl-process ()
  "Return inferior Coffee process or nil."
  (get-buffer-process (coffee-repl-buffer)))

;;;###autoload
(define-derived-mode coffee-repl-mode comint-mode "Coffee"
  "Major mode for Coffee repl.

\\<coffee-repl-mode-map>"
  (setq-local comint-input-ignoredups t
              comint-prompt-read-only t
              comint-prompt-regexp "^coffee> "
              comint-highlight-input nil
              comint-process-echoes t
              comint-preoutput-filter-functions
              '(xterm-color-filter coffee-repl--preoutput-filter))
  (add-hook 'comint-preoutput-filter-functions 'coffee-comint-filter nil t))

(defun nvp-coffee-repl (show &rest _)
  "Return coffee repl process, starting a new one when there isnt one.
If SHOW, pop to repl buffer."
  (interactive (list t))
  (let ((buffer (coffee-repl-buffer)))
    (unless buffer
      (setq buffer (apply #'make-comint-in-buffer
                          "coffee" coffee-repl-buffer
                          "env" nil
                          "NODE_NO_READLINE=1"
                          "TERM=xterm"
                          coffee-command coffee-args-repl))
      (with-current-buffer buffer
        (coffee-repl-mode)))
    (prog1 (get-buffer-process buffer)
      (and show (pop-to-buffer buffer)))))

(with-eval-after-load 'nvp-repl
  (nvp-repl-add '(coffee-mode coffee-ts-mode)
    :name 'coffee
    :modes '(coffee-repl-mode)
    :init #'nvp-coffee-repl
    :find-fn #'coffee-repl-buffer
    :history-file ".coffee_history"))


(provide 'coffee-repl)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; coffee-repl.el ends here
