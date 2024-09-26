;;; nvp-search.el --- search-map; search/replace -*- lexical-binding: t; -*-
;;; Code:
(eval-when-compile (require 'nvp-macro))
(require 'transient)
(nvp:decls :f (occur-read-primary-args))

(autoload 'nvp-buffer-matching-mode "nvp-buffer")
(autoload 'multi-occur "replace")
(autoload 'multi-occur-in-matching-buffers "replace")

;;;###autoload(autoload 'nvp-search-buffers-menu "nvp-search" nil t)
(transient-define-prefix nvp-search-buffers-menu ()
  "Search buffers"
  [["Goto line"
    ("l" "Matching" consult-line)
    ("L" "in Project" consult-line-multi)]
   ["Imenu"
    ("i" "Imenu" consult-imenu)
    ("I" "in Project" consult-imenu-multi)]
   ["Occur"
    ("o" "Dwim" nvp-occur-dwim)
    ("O" "in Mode" nvp-multi-occur-in-this-mode)]])

;;;###autoload
(defun nvp-occur-dwim (&optional arg)
  "Call `occur' with either region or `symbol-at-point'.
With prefix ARG multi-occur in buffers of the same mode."
  (interactive "P")
  (when-let* ((str (nvp:tap 'dwim)))
    (push (if (stringp str)
              (regexp-quote str)
            str)
          regexp-history))
  (call-interactively
   (pcase arg
     ((or 0 '(4))
      (or (zerop arg) (nvp:prefix-shift -1))
      #'nvp-multi-occur-in-this-mode)
     (`(16)
      (nvp:prefix-shift -2)
      #'multi-occur-in-matching-buffers)
     (_ #'occur))))

;;;###autoload
(defun nvp-multi-occur-in-this-mode (&optional mode nlines)
  "Occur in buffers with same major mode as current.
With prefix of 0, prompt for MODE. Other prefixes set NLINES for `occur',
which see."
  (interactive (list (and (zerop current-prefix-arg)
                          (nvp-read-mode "Occur in mode: "))
                     (prefix-numeric-value current-prefix-arg)))
  (multi-occur (nvp-buffer-matching-mode (or mode major-mode))
               (car (occur-read-primary-args))
               nlines))

(provide 'nvp-search)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; nvp-search.el ends here
