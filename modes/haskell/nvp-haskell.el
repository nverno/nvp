;;; nvp-haskell.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp:decls :p ("haskell"))

(defun nvp-haskell-arrow ()
  (interactive)
  (nvp-cycle (nvp:input 'lcs) '("_" " -> ")))

;; Make compilation-mode understand "at blah.hs:11:34-50" lines
;; output by GHC
(with-eval-after-load 'compile
  (let ((alias 'ghc-at-regexp))
    (add-to-list
     'compilation-error-regexp-alist-alist
     (list
      alias
      " at \\(.*\\.\\(?:l?[gh]hs\\|hi\\)\\):\\([0-9]+\\):\\([0-9]+\\)-[0-9]+$"
      1 2 3 0 1))
    (add-to-list 'compilation-error-regexp-alist alias)))

;;; Align
;; https://github.com/jwiegley/dot-emacs/init.el#L1954
(with-eval-after-load 'align
  '(nconc
    align-rules-list
    (mapcar #'(lambda (x)
                `(,(car x)
                  (regexp . ,(cdr x))
                  (modes quote (haskell-mode literate-haskell-mode))))
            '((haskell-types       . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
              (haskell-assignment  . "\\(\\s-+\\)=\\s-+")
              (haskell-arrows      . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
              (haskell-left-arrows . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")))))

;;; Help 
(defvar nvp-hoogle-server-process nil)
(defun nvp-haskell-hoogle (query &optional _arg)
  "Do a Hoogle search for QUERY."
  (interactive
   (let ((def (haskell-ident-at-point)))
     (if (and def (symbolp def)) (setq def (symbol-name def)))
     (list (read-string (if def
                            (format "Hoogle query (default %s): " def)
                          "Hoogle query: ")
                        nil nil def)
           current-prefix-arg)))
  (unless (and nvp-hoogle-server-process
               (process-live-p nvp-hoogle-server-process))
    (message "Starting local Hoogle server on port 8687...")
    (with-current-buffer (get-buffer-create " *hoogle-web*")
      (cd temporary-file-directory)
      (setq nvp-hoogle-server-process
            (start-process "hoogle-web" (current-buffer)
                           (executable-find "hoogle")
                           "server" "--local" "--port=8687")))
    (message "Starting local Hoogle server on port 8687...done"))
  (browse-url
   ;; url-hexify-string?
   (format "http://127.0.0.1:8687/?hoogle=%s"
           (replace-regexp-in-string
            " " "+" (replace-regexp-in-string "\\+" "%2B" query)))))

(provide 'nvp-haskell)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-haskell.el ends here
