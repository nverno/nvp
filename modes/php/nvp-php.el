;;; nvp-php.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)

;; (defun nvp-php-compile ()
;;   (interactive)
;;   (let ((compilation-read-command nil))
;;    (compile (concat php-executable " -f " (buffer-file-name)))))

;; (defun nvp-php-compile-trim (&optional arg)
;;   "Send the buffer to php executable, stripping the header and creating a 
;; temporary file to compile."
;;   (interactive "P")
;;   (goto-char (point-min))
;;   (re-search-forward (regexp-quote "<?php") nil t)
;;   (let* ((start(or (and (eobp) (point-min))
;;                    (match-end 0)))
;;          (end (point-max))
;;          (tempfile (make-temp-file (buffer-file-name)))
;;          (compilation-read-command current-prefix-arg))
;;     (write-region start end tempfile)
;;     (with-current-buffer (find-file-noselect tempfile)
;;       (compile (shell-quote-argument
;;                 (concat (format "%s -f %s"
;;                                 (w32-convert-standard-filename php-executable)
;;                                 (w32-convert-standard-filename tempfile))))))
;;     (delete-file tempfile)))

(provide 'nvp-php)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-php.el ends here
