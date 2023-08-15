;;; nvp-jq.el --- jq things -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'jq-mode nil t)
(nvp:decls :f (json-mode jq-interactively))

;;;###autoload
(defun nvp-jq (beg end &optional buffer)
  "Run `jq-interactively', but don't hide/change the original buffer.
BEG and END specify region to run.
With prefix PROMPT, prompt for name of output buffer."
  (interactive
   (let ((bnds (if (region-active-p)
                   (list (region-beginning)
                         (region-end))
                 (list (point-min)
                       (point-max)))))
     (if current-prefix-arg
         (append bnds (list (read-buffer "Out buffer: ")))
       bnds)))
  (let ((buf (current-buffer))
        (out-buf (or buffer (format "*jq-shell[%s]*" (nvp:bfn)))))
    (with-current-buffer (get-buffer-create out-buf)
      (erase-buffer)
      (insert-buffer-substring buf (or beg (point-min)) (or end (point-max)))
      (json-mode)
      (goto-char (point-min))
      (display-buffer (current-buffer))
      (call-interactively #'jq-interactively))))


(provide 'nvp-jq)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-jq.el ends here
