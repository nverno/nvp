;;; nvp-haskell.el ---  -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Last modified: <2019-03-22 01:27:19>
;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nvp
;; Created: 21 March 2019

;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'nvp-macro)
  (require 'cl-lib))
(require 'nvp)
(declare-function haskell-ident-at-point "")

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
  (let ((pe process-environment)
        (ep exec-path))
    (unless (and nvp-hoogle-server-process
                 (process-live-p nvp-hoogle-server-process))
      (message "Starting local Hoogle server on port 8687...")
      (with-current-buffer (get-buffer-create " *hoogle-web*")
        (cd temporary-file-directory)
        (let ((process-environment pe)
              (exec-path ep))
          (setq nvp-hoogle-server-process
                (start-process "hoogle-web" (current-buffer)
                               (executable-find "hoogle")
                               "server" "--local" "--port=8687"))))
      (message "Starting local Hoogle server on port 8687...done")))
  (browse-url
   (format "http://127.0.0.1:8687/?hoogle=%s"
           (replace-regexp-in-string
            " " "+" (replace-regexp-in-string "\\+" "%2B" query)))))

(provide 'nvp-haskell)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-haskell.el ends here
