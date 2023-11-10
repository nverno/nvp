;;; nvp-prolog.el --- prolog -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp:decls :p (flymake))

;; https://www.metalevel.at/pceprolog/
(defun nvp-prolog-insert-comment-block ()
  "Insert a PceEmacs-style comment block like /* - - ... - - */ "
  (interactive)
  (let ((dashes "-"))
    (dotimes (_ 36) (setq dashes (concat "- " dashes)))
    (insert (format "/* %s\n\n%s */" dashes dashes))
    (forward-line -1)
    (indent-for-tab-command)))

;;; Flymake

(defun nvp-prolog-flymake-init ()
  (let* ((temp-file
          (flymake-init-create-temp-buffer-copy #'flymake-create-temp-inplace))
         (local-file
          (file-relative-name temp-file (file-name-directory buffer-file-name))))
    (list "swipl" (list "-q" "-t" "halt" "-s " local-file))))

(defun nvp-prolog-flymake-setup ()
  (require 'flymake)
  (make-local-variable 'flymake-allowed-file-name-masks)
  (make-local-variable 'flymake-err-line-patterns)
  (setq flymake-err-line-patterns
        '(("ERROR: (?\\(.*?\\):\\([0-9]+\\)" 1 2)
          ("Warning: (\\(.*\\):\\([0-9]+\\)" 1 2)))
  (setq flymake-allowed-file-name-masks '(("\\.pl\\'" nvp-prolog-flymake-init)))
  (flymake-mode 1))

(provide 'nvp-prolog)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-prolog.el ends here
