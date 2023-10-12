;;; nvp-search.el --- search-map; search/replace -*- lexical-binding: t; -*-
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'nvp)
(nvp:decls :f (occur-read-primary-args))
(nvp:auto "nvp-buffer" 'nvp-buffer-matching-mode)
(nvp:auto "replace" 'multi-occur 'multi-occur-in-matching-buffers)

;; -------------------------------------------------------------------
;;; Occur 

;;;###autoload
(defun nvp-occur-dwim (&optional arg)
  "Call `occur' with either region or symbol-at-point.
With prefix ARG multi-occur in buffers of the same mode."
  (interactive "P")
  (when-let* ((str (nvp:tap 'dwim)))
    (push (if (stringp str) (regexp-quote str) str) regexp-history))
  (pcase arg
    ('nil (call-interactively #'occur))
    (`(4) (nvp-multi-occur-in-this-mode))
    (`(16)
     (let (current-prefix-arg)
       (call-interactively #'multi-occur-in-matching-buffers)))
    (_ (call-interactively #'occur)))
  (if (get-buffer "*Occur*")
      (switch-to-buffer-other-window "*Occur*")))

;;;###autoload
(defun nvp-multi-occur-in-this-mode ()
  "Occur in buffers with same major mode as current."
  (interactive)
  (multi-occur
   (nvp-buffer-matching-mode major-mode) (car (occur-read-primary-args))))

(provide 'nvp-search)
;;; nvp-search.el ends here
