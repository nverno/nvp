;;; nvp-r-spin.el --- spin rmarkdown -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))

(declare-function ess-send-string "ess-inf")
(declare-function ess-get-words-from-vector "ess-inf")
(declare-function browse-url-file-url "browse-url")

;; spin yarn and show result in browser or open if it is a pdf
;; use ess-process when available, otherwise try rscript
;;;###autoload
(defun nvp-r-spin-file (file)
  (interactive
   (list (or (and (eq major-mode 'ess-r-mode) buffer-file-name)
             (read-file-name "File to spin: "))))
  (let ((proc (and (fboundp 'ess-get-process)
                   (ignore-errors (ess-get-process nil t))))
        (cmd (nvp:concat "local({ "
                         "out <- rmarkdown::render(\"%s\", \"all\");"
                         "if (tools::file_ext(out) == \"html\") {"
                         "    browseURL(out);"
                         "} else {"
                         "    out;"
                         "}"
                         "})\n")))
    (if proc
        (let ((res (ess-get-words-from-vector (format cmd file))))
          (pcase (file-name-extension (car res))
            (`"pdf" (find-file-other-window (car res)))
            (_ (browse-url-file-url (car res)))))
      ;; FIXME: not tested, doesn't open or return pdf
      (start-process-shell-command
       "rscript" nil
       (concat "Rscript -e \"" (format cmd file) "\"")))))

(provide 'nvp-r-spin)
;;; nvp-r-spin.el ends here
